module Main exposing (..)

import Problem.Example exposing (Queens, SlidingPuzzle, complexEightPuzzle, mediumEightPuzzle, queens, romania, simpleEightPuzzle, simpleRouteFinding, slidingPuzzleVisual)
import Problem.Search as Search exposing (Result(..))
import Problem.Search.Dashboard as Dashboard exposing (Search(..), Visual(..))


main =
    Dashboard.document
        { problem = simpleRouteFinding
        , searches = [ UniformCost, BestFirst, Greedy ]
        , visuals = [ Scatter, Tree, TreeMap, Graph ]
        }
