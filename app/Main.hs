{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import MyLib

type Position = Schema "position" '["x" ::: Int]

type Velocity = Schema "velocity" '["v" ::: Int]

main :: IO ()
main =
  let q =
        (fetch @Position `as` #p ? fetch @Velocity `as` #v)
          `returning` (#p :. #x :& #v :. #v)
   in print $ encodeQuery q
