{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Aztecs.ECS hiding (Query, fetch)
import Aztecs.Script hiding (Component)
import Control.Monad
import Control.Monad.IO.Class
import Data.Data

type Position = Schema "position" '["x" ::: Int]

type Velocity = Schema "velocity" '["v" ::: Int]

newtype PositionC = Position Int deriving (Show, Data)

instance Component PositionC

instance ScriptComponent PositionC where
  getField "x" (Position x) = Just $ IntPrimitive x
  getField s _ = error s

newtype VelocityC = Velocity Int 

instance Component VelocityC

instance ScriptComponent VelocityC where
  getField "v" (Velocity v)  = Just $ IntPrimitive v
  getField s _ = error s

run :: SystemT IO ()
run = do
  let rt = insertComponent @PositionC "position" $ insertComponent @VelocityC "velocity" mempty
      script =
        (fetch @Position `as` #p ? fetch @Velocity `as` #v)
          `returning` (#p :. #x :& #v :. #v)
      decoder = decodeQuery (encodeQuery script)
      q = buildDecoder decoder rt
  positions <- fromSystem $ query q
  liftIO $ print positions

app :: AccessT IO ()
app = do
  _ <- spawn $ bundle (Position 0) <> bundle (Velocity 1)
  forever $ system run

main :: IO ()
main = runAccessT_ app
