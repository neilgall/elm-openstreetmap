module TestUtil exposing (..)

import Expect exposing (..)

expectEqualWithin : Float -> Float -> Float -> Expectation
expectEqualWithin delta a b = Expect.lessThan delta (abs (a - b))

expectInRange : comparable -> comparable -> comparable -> Expectation
expectInRange min max x =
  expectAll [ greaterThan min x, lessThan max x ]

expectAll : List Expectation -> Expectation
expectAll expectations =
  case expectations of
    [] ->
      Expect.pass

    x :: xs ->
      case Expect.getFailure x of
        Nothing -> expectAll xs
        Just _  -> x
