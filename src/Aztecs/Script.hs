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
import Data.Data
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.OverloadedLabels
import GHC.TypeLits
import Text.Parsec
import Text.Parsec.String

data Schema (a :: Symbol) b

data (a :: Symbol) ::: b

class HasField (a :: Symbol) b

instance {-# OVERLAPPING #-} HasField a ((a ::: b) ': s)

instance {-# OVERLAPPING #-} HasField a ('[a ::: b])

instance (HasField a s) => HasField a (b ': s)

instance (HasField a bs) => HasField a (Schema s bs)

class Queryable a where
  queryId :: String

instance (KnownSymbol a) => Queryable (Schema a b) where
  queryId = symbolVal (Proxy @a)

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

instance (KnownSymbol a, KnownSymbol b, HasField b (KnownAliasT s a)) => Row s (a :. b) where
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
  Fetch :: (Queryable a) => Query '[] a
  As :: (KnownSymbol s') => Query s a -> Alias s' -> Query ('(s', a) ': s) ()
  Returning :: (Row s r) => Query s a -> r -> Query s ()
  And :: Query s () -> Query s' () -> Query (s ++ s') ()

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ b = b
  (a ': as) ++ b = a ': (as ++ b)

fetch :: (Queryable a) => Query '[] a
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

data FieldAccess = FieldAccess String String
  deriving (Show, Eq)

data Decoder
  = FetchDecoder String
  | AsDecoder Decoder String
  | AndDecoder Decoder Decoder
  | ReturningDecoder Decoder [FieldAccess]
  deriving (Show, Eq)

skipSpaces :: Parser ()
skipSpaces = skipMany (oneOf " \t")

skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 (oneOf " \t")

simpleDecoderParser :: Parser Decoder
simpleDecoderParser = do
  skipSpaces
  _ <- string "FETCH"
  skipSpaces1
  qId <- many1 (noneOf " \t\n\r")
  let base = FetchDecoder qId
  aliases <- many (try asClauseParser)
  return $ foldl AsDecoder base aliases

asClauseParser :: Parser String
asClauseParser = try $ do
  skipSpaces
  _ <- string "AS"
  skipSpaces1
  alias <- many1 (noneOf " \t\n\r")
  return alias

andChainParser :: Parser Decoder
andChainParser = do
  first <- simpleDecoderParser
  rest <- many $ try $ do
    skipSpaces
    _ <- string "AND"
    skipSpaces
    simpleDecoderParser
  return $ foldl AndDecoder first rest

fieldAccessParser :: Parser FieldAccess
fieldAccessParser = do
  skipSpaces
  alias <- many1 (noneOf ". ,()")
  _ <- char '.'
  field <- many1 (noneOf " ,()")
  return $ FieldAccess alias field

returningFieldsParser :: Parser [FieldAccess]
returningFieldsParser = fieldAccessParser `sepBy1` (skipSpaces >> char ',' >> skipSpaces)

returningClauseParser :: Parser [FieldAccess]
returningClauseParser = do
  skipSpaces
  _ <- string "RETURNING"
  skipSpaces
  _ <- char '('
  fields <- returningFieldsParser
  _ <- char ')'
  return fields

decoderParser :: Parser Decoder
decoderParser = do
  chain <- andChainParser
  ret <- optionMaybe returningClauseParser
  skipSpaces
  eof
  case ret of
    Just r -> return (ReturningDecoder chain r)
    Nothing -> return chain

decodeQuery :: String -> Decoder
decodeQuery input =
  case parse decoderParser "" input of
    Left err -> error ("Failed to parse query: " ++ show err)
    Right dec -> dec

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

buildDecoder :: Decoder -> Runtime -> Q.Query [Primitive]
buildDecoder dec rt = (\(_, dyn, _) -> fromMaybe [] $ fromDynamic dyn) <$> buildDecoder' dec rt

buildDecoder' :: Decoder -> Runtime -> Q.Query (String, Dynamic, Scope)
buildDecoder' (FetchDecoder qId) rt = case unRuntime rt Map.!? qId of
  Just cp -> (qId,,mempty) <$> queryComponentProxy cp
  Nothing -> error "TODO"
buildDecoder' (AsDecoder dec ident) rt =
  let q = buildDecoder' dec rt
   in fmap (\(qId, dyn, scope) -> ("", toDyn (), Scope $ Map.insert ident (qId, dyn) (unScope scope))) q
buildDecoder' (AndDecoder a b) rt =
  let q = buildDecoder' a rt
      q' = buildDecoder' b rt
   in (\(_, _, scope) (_, _, scope') -> ("", toDyn (), scope' <> scope)) <$> q <*> q'
buildDecoder' (ReturningDecoder dec fields) rt =
  ( \(_, _, scope) ->
      let dyns =
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
       in ("", toDyn dyns, scope)
  )
    <$> buildDecoder' dec rt

data Primitive = IntPrimitive Int
  deriving (Show, Eq)

class ScriptComponent a where
  getField :: String -> a -> Maybe Primitive
