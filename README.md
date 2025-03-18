# aztecs-script

A scripting language for the [Aztecs](https://github.com/aztecs-hs/aztecs) ECS and game-engine.

Aztecs-script aims to be a turing-complete query language that can be used
for both scripting gameplay and controlling the ECS over a network.
This package provides both fully-typed Haskell DSL for scripting as well as a low-level interpreter for the text-based scripting language.

#### Haskell:

```hs
fetch @Position `as` #p <?> fetch @Velocity `as` #v `returning` #p :. #x :& #v :. #v
```

#### aztecs-script:

```sql
FETCH position AS p AND FETCH velocity AS v RETURNING (p.x, v.v)
```

## Features

- Fully-typed DSL and text-based language
- Queries to the ECS
  - Reading
  - (TODO) Writing
- (TODO) Observers and event triggers
- (TODO) Control-flow
- (TODO) Primtive functions and data-types

## Full example

```hs
import Aztecs.ECS hiding (Query, fetch)
import Aztecs.Script hiding (Component)
import Aztecs.Script.Decoder
import Aztecs.Script.Interpreter
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
    fetch @Position `as` #p <?> fetch @Velocity `as` #v `returning` #p :. #x :& #v :. #v

run :: SystemT IO ()
run = do
  let rt = export @Position "position" <> export @Velocity "velocity"
      q = buildQuery script rt
  positions <- fromSystem $ query q
  liftIO $ print positions

app :: AccessT IO ()
app = do
  _ <- spawn $ bundle (Position 0) <> bundle (Velocity 1)
  forever $ system run

main :: IO ()
main = runAccessT_ app
```

## Inspiration

- [Squeal](https://github.com/morphismtech/squeal): An incredible Haskell DSL for SQL
- [Flecs query language](https://github.com/SanderMertens/flecs/blob/master/docs/FlecsQueryLanguage.md):
  Awesome SQL-like ECS query language
