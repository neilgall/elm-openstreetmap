module Util exposing (..)

import String.Extra

stringWithSubstitutions : String -> List (String, String) -> String
stringWithSubstitutions str subs =
  let replace (a, b) = String.Extra.replace a b in
  List.foldr replace str subs

px : Int -> String
px n = toString n ++ "px"
