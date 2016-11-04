module TestUtil exposing (..)

import Expect exposing (..)

expectEqualWithin : Float -> Float -> Float -> Expectation
expectEqualWithin delta a b = Expect.lessThan delta (abs (a - b))

expectAll : List Expectation -> Expectation
expectAll expectations =
  case expectations of
    [] ->
      Expect.pass

    x :: xs ->
      case Expect.getFailure x of
        Nothing -> expectAll xs
        Just _  -> x
