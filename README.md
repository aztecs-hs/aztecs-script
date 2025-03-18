# aztecs-script

A scripting language for the [Aztecs](https://github.com/aztecs-hs/aztecs) ECS and game-engine.

```hs
import Aztecs.Script

type Position = Schema "position" '["x" ::: Int]

type Velocity = Schema "velocity" '["v" ::: Int]

main :: IO ()
main =
  let q =
        (fetch @Position `as` #p ? fetch @Velocity `as` #v)
          `returning` (#p :. #x :& #v :. #v)
   in print $ encodeQuery q
```
