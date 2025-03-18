{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Aztecs.ECS hiding (Query, fetch)
import Aztecs.Script hiding (Component)
import Control.Monad
import Control.Monad.IO.Class

newtype Position = Position Int deriving (Show)

instance Component Position

instance ScriptComponent Position where
  type ComponentID Position = "position"
  type Schema Position = '["x" ::: Int]

  getField "x" (Position x) = Just $ IntPrimitive x
  getField _ _ = Nothing

newtype Velocity = Velocity Int

instance Component Velocity

instance ScriptComponent Velocity where
  type ComponentID Velocity = "velocity"
  type Schema Velocity = '["v" ::: Int]

  getField "v" (Velocity v) = Just $ IntPrimitive v
  getField _ _ = Nothing

script :: String
script =
  encodeQuery $
    (fetch @Position `as` #p ? fetch @Velocity `as` #v)
      `returning` (#p :. #x :& #v :. #v)

run :: SystemT IO ()
run = do
  let rt = insertComponent @Position "position" $ insertComponent @Velocity "velocity" mempty
      q = buildQuery script rt
  positions <- fromSystem $ query q
  liftIO $ print positions

app :: AccessT IO ()
app = do
  _ <- spawn $ bundle (Position 0) <> bundle (Velocity 1)
  forever $ system run

main :: IO ()
main = runAccessT_ app
