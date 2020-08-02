module Benchmarks exposing (..)

import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Search exposing (breadthFirstSearch, depthFirstSearch, heuristicSearch)
import Search.Problems.NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle)
import Search2 exposing (breadthFirstSearch, depthFirstSearch, heuristicSearch)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "search"
        [ describe "old"
            [ benchmark "breadth-first search" <|
                \_ -> Search.breadthFirstSearch simpleEightPuzzle
            , benchmark "heuristic search simple" <|
                \_ -> Search.heuristicSearch simpleEightPuzzle
            , benchmark "heuristic search medium" <|
                \_ -> Search.heuristicSearch mediumEightPuzzle
            ]

        {- , describe "new"
           [ benchmark "breadth-first search" <|
               \_ -> Search2.breadthFirstSearch simpleEightPuzzle
           , benchmark "heuristic search simple" <|
               \_ -> Search2.heuristicSearch simpleEightPuzzle
           , benchmark "heuristic search medium" <|
               \_ -> Search2.heuristicSearch mediumEightPuzzle
           ]
        -}
        ]
