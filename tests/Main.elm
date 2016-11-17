port module Main exposing (..)

import BSTSetTests
import Json.Encode exposing (Value)
import Test.Runner.Node exposing (run, TestProgram)


main : TestProgram
main =
    run emit BSTSetTests.all


port emit : ( String, Value ) -> Cmd msg
