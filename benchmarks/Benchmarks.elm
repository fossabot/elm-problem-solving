module Benchmarks exposing (..)

import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Search exposing (breadthFirstSearch, depthFirstSearch)
import Search.NPuzzle exposing (simpleEightPuzzle)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        sampleArray =
            Array.initialize 100 identity
    in
    describe "Search"
        [ 
          describe "NPuzzle"
            [ benchmark "depth-first search" <|
                \_ -> breadthFirstSearch simpleEightPuzzle
            , benchmark "breadth-first search" <|
                \_ -> breadthFirstSearch simpleEightPuzzle
            ]
        ]
