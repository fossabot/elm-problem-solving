module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Math exposing (..)
import Test exposing (..)
import Search exposing (..)
import Search.VacuumWorld exposing (..)
import Search.EightPuzzle exposing (..)

suite : Test
suite = 
    describe "Aggregated tests from all modules" [
        describe "The EightPuzzle module" [
            Search.EightPuzzle.suite ()
        ]
    ]