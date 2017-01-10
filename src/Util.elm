module Util exposing (..)

import Debug
import String.Extra

stringWithSubstitutions : String -> List (String, String) -> String
stringWithSubstitutions str subs =
  let replace (a, b) = String.Extra.replace a b in
  List.foldr replace str subs

frac : Float -> Float
frac x = x - toFloat (truncate x)

roundDiv : Int -> Int -> Int
roundDiv n m = (n + m - 1) // m

assert : String -> (a -> Bool) -> a -> a
assert message condition value =
  if (not <| condition value)
    then Debug.crash ("Assertion failed: " ++ message)
    else value
