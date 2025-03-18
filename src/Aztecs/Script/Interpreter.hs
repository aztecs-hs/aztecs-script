{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.Script.Interpreter
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Script.Interpreter
  ( -- * Interpreter
    Interpreter (..),
    export,
    buildQuery,

    -- * Internal
    Scope (..),
    buildDecoder,
  )
where

import qualified Aztecs.ECS.Component as C
import qualified Aztecs.ECS.Query as Q
import Aztecs.Script.Decoder
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

newtype Interpreter = Interpreter {unInterpreter :: Map String ComponentProxy}
  deriving (Semigroup, Monoid)

export :: forall a. (C.Component a, ScriptComponent a) => String -> Interpreter
export name = Interpreter $ Map.singleton name (ComponentProxy (Proxy @a))

newtype Scope = Scope {unScope :: Map String (String, DynamicScriptComponent)}
  deriving (Semigroup, Monoid)

buildQuery :: String -> Interpreter -> Q.Query [Primitive]
buildQuery = buildDecoder . decodeQuery

buildDecoder :: Decoder [Primitive] -> Interpreter -> Q.Query [Primitive]
buildDecoder dec rt = fst <$> buildDecoder' dec rt

buildDecoder' :: Decoder a -> Interpreter -> Q.Query (a, Scope)
buildDecoder' (FetchDecoder qId) rt = case unInterpreter rt Map.!? qId of
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
                  Just (_, dyn) -> case getFieldDyn f dyn of
                    Just d -> d
                    Nothing -> error ""
                  Nothing -> error ""
              )
              fields
       in (primitives, scope)
  )
    <$> buildDecoder' dec rt

data ComponentProxy = forall a. (C.Component a, ScriptComponent a) => ComponentProxy (Proxy a)

queryProxy :: (C.Component a) => Proxy a -> Q.Query a
queryProxy _ = Q.fetch

queryComponentProxy :: ComponentProxy -> Q.Query DynamicScriptComponent
queryComponentProxy (ComponentProxy p) = DynamicScriptComponent <$> queryProxy p
