module Main exposing (main)

import Problem.Example exposing (..)
import Problem.Search as Search exposing (Result(..))
import Problem.Search.Dashboard as Dashboard exposing (Search(..), Visual(..))


main =
    Dashboard.document
        { problem = simpleEightPuzzle

        -- problemStateToHtml = Nothing
        , problemStateToHtml = Just slidingPuzzleVisual
        , searches = [ Greedy ]
        , visuals = [ Scatter, Tree, TreeMap, Graph ]
        }



{-
   Problem templates:
    - simpleEightPuzzle, mediumEightPuzzle, complexEghtPuzzle, slidingPuzzle 15
    - queens 4, queens 8
    - simpleKnuth
    - complexKnuth
    - simpleRouteFinding
    - simpleMotionPlanning
   (For the problems other than the sliding puzzle, we need to set `problemStateToHtml` to `Nothing`, because I have not yet coded visualizations of their states.)

   Search techniques (graph and tree search):
    - DepthFirst, TreeDepthFirst
    - BreadthFirst, TreeBreadthFirst
    - UniformCost, TreeUniformCost (=Dijkstra)
    - Greedy, TreeGreedy
    - BestFirst, TreeBestFirst

   Visuals:
    - Scatter (x: depth, y: heuristic)
    - Tree
    - TreeMap
    - Graph

   Documentation:
   https://package.elm-lang.org/packages/davidpomerenke/elm-problem-solving/latest/

   Github:
   https://github.com/davidpomerenke/elm-problem-solving
-}
