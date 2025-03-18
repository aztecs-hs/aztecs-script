{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.Script where

import qualified Aztecs.ECS.Component as C
import qualified Aztecs.ECS.Query as Q
import Aztecs.Script.Decoder
import Data.Data
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.OverloadedLabels
import GHC.TypeLits

data (a :: Symbol) ::: b

class HasField (a :: Symbol) b

instance {-# OVERLAPPING #-} HasField a ((a ::: b) ': s)

instance {-# OVERLAPPING #-} HasField a ('[a ::: b])

instance (HasField a s) => HasField a (b ': s)

class KnownAlias (s :: [Symbol]) a

instance {-# OVERLAPPING #-} (KnownSymbol a) => KnownAlias (a ': s) (Alias a)

instance {-# OVERLAPPING #-} (KnownSymbol a) => KnownAlias '[a] (Alias a)

instance {-# OVERLAPPING #-} (KnownAlias s a) => KnownAlias (a' ': s) a

infixr 3 :&

data a :& b = a :& b

class Row s a where
  encodeRow :: a -> String

newtype Component a = Component {unComponent :: Alias a}

component :: (KnownSymbol a) => Alias a -> Component a
component = Component

infixr 5 :.

data a :. b = (:.) (Alias a) (Alias b)

instance (KnownSymbol a, KnownSymbol b, HasField b (Schema (KnownAliasT s a))) => Row s (a :. b) where
  encodeRow (a :. b) = aliasVal a ++ "." ++ aliasVal b

instance (KnownSymbol s') => Row s (Component s') where
  encodeRow = aliasVal . unComponent

instance (Row s a, Row s b) => Row s (a :& b) where
  encodeRow (a :& b) = encodeRow @s a ++ ", " ++ encodeRow @s b

type family KnownAliasT (scope :: [(Symbol, Type)]) (s :: Symbol) where
  KnownAliasT '[ '(s, a)] s = a
  KnownAliasT ('(s, a) ': scope) s = a
  KnownAliasT (_ ': scope) s = KnownAliasT scope s

data Query (s :: [(Symbol, Type)]) (a :: Type) where
  Fetch :: (ScriptComponent a) => Query '[] a
  As :: (KnownSymbol s') => Query s a -> Alias s' -> Query ('(s', a) ': s) ()
  Returning :: (Row s r) => Query s a -> r -> Query s ()
  And :: Query s () -> Query s' () -> Query (s ++ s') ()

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ b = b
  (a ': as) ++ b = a ': (as ++ b)

fetch :: (ScriptComponent a) => Query '[] a
fetch = Fetch

as :: (KnownSymbol s') => Query s a -> Alias s' -> Query ('(s', a) ': s) ()
as = As

returning :: (Row s r) => Query s a -> r -> Query s ()
returning = Returning

infixr 5 ?

(?) :: Query s () -> Query s' () -> Query (s ++ s') ()
(?) = And

data Alias (alias :: Symbol) = Alias deriving (Eq, Ord, Show)

aliasVal :: forall s. (KnownSymbol s) => Alias s -> String
aliasVal _ = symbolVal (Proxy @s)

instance (alias1 ~ alias2) => IsLabel alias1 (Alias alias2) where
  fromLabel = Alias

encodeQuery :: forall s a. Query s a -> String
encodeQuery Fetch = "FETCH " ++ queryId @a
encodeQuery (As q a) = encodeQuery q ++ " AS " ++ aliasVal a
encodeQuery (Returning q r) = encodeQuery q ++ " RETURNING (" ++ encodeRow @s r ++ ")"
encodeQuery (And a b) = encodeQuery a ++ " AND " ++ encodeQuery b

data ComponentProxy = forall a. (C.Component a, ScriptComponent a) => ComponentProxy (Proxy a)

queryProxy :: (C.Component a) => Proxy a -> Q.Query a
queryProxy _ = Q.fetch

queryComponentProxy :: ComponentProxy -> Q.Query Dynamic
queryComponentProxy (ComponentProxy p) = toDyn <$> queryProxy p

getFieldComponentProxy :: String -> ComponentProxy -> Dynamic -> Maybe Primitive
getFieldComponentProxy fieldName (ComponentProxy p) d = getFieldProxy fieldName p d

getFieldProxy :: forall a. (Typeable a, ScriptComponent a) => String -> Proxy a -> Dynamic -> Maybe Primitive
getFieldProxy fieldName _ d = case fromDynamic @a d of
  Just a -> getField fieldName a
  Nothing -> error "TODO"

newtype Runtime = Runtime {unRuntime :: Map String ComponentProxy}
  deriving (Semigroup, Monoid)

insertComponent :: forall a. (C.Component a, ScriptComponent a) => String -> Runtime -> Runtime
insertComponent name rt = Runtime $ Map.insert name (ComponentProxy (Proxy @a)) (unRuntime rt)

newtype Scope = Scope {unScope :: Map String (String, Dynamic)}
  deriving (Show, Semigroup, Monoid)

buildQuery :: String -> Runtime -> Q.Query [Primitive]
buildQuery = buildDecoder . decodeQuery

buildDecoder :: Decoder [Primitive] -> Runtime -> Q.Query [Primitive]
buildDecoder dec rt = fst <$> buildDecoder' dec rt

buildDecoder' :: Decoder a -> Runtime -> Q.Query (a, Scope)
buildDecoder' (FetchDecoder qId) rt = case unRuntime rt Map.!? qId of
  Just cp -> (\dyn -> ((qId, dyn), mempty)) <$> queryComponentProxy cp
  Nothing -> error "TODO"
buildDecoder' (AsDecoder dec ident) rt =
  let q = buildDecoder' dec rt
   in fmap (\((qId, dyn), scope) -> ((), Scope $ Map.insert ident (qId, dyn) (unScope scope))) q
buildDecoder' (AndDecoder a b) rt =
  let q = buildDecoder' a rt
      q' = buildDecoder' b rt
   in (\(_, scope) (_, scope') -> ((), scope' <> scope)) <$> q <*> q'
buildDecoder' (ReturningDecoder dec fields) rt =
  ( \(_, scope) ->
      let primitives =
            map
              ( \(FieldAccess record f) -> case unScope scope Map.!? record of
                  Just (ty, dyn) -> case unRuntime rt Map.!? ty of
                    Just cp -> case getFieldComponentProxy f cp dyn of
                      Just d -> d
                      Nothing -> error ""
                    Nothing -> error ""
                  Nothing -> error ""
              )
              fields
       in (primitives, scope)
  )
    <$> buildDecoder' dec rt

class (KnownSymbol (ComponentID a)) => ScriptComponent a where
  type ComponentID a :: Symbol

  type Schema a :: [Type]

  getField :: String -> a -> Maybe Primitive

  queryId :: String
  queryId = symbolVal (Proxy @(ComponentID a))
