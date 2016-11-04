module Util exposing (..)

import String.Extra

stringWithSubstitutions : String -> List (String, String) -> String
stringWithSubstitutions str subs =
  let replace (a, b) = String.Extra.replace a b in
  List.foldr replace str subs

px : Int -> String
px n = toString n ++ "px"

frac : Float -> Float
frac x = x - toFloat (truncate x)

roundDiv : Int -> Int -> Int
roundDiv n m = (n + m - 1) // m
