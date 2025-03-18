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

-- |
-- Module      : Aztecs.Script
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Script
  ( (:::),
    HasField,
    KnownAlias,
    KnownAliasT,
    (:&) (..),
    Component (..),
    component,
    (:.) (..),
    Row (..),
    Query (..),
    fetch,
    as,
    returning,
    (<?>),
    encodeQuery,
  )
where

import Aztecs.Script.Decoder
import Data.Data
import Data.Kind
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

infixr 2 `returning`

returning :: (Row s r) => Query s a -> r -> Query s ()
returning = Returning

infixr 5 <?>

(<?>) :: Query s () -> Query s' () -> Query (s ++ s') ()
(<?>) = And

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
