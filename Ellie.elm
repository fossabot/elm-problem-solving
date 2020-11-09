module Main exposing (main)

import Problem exposing (Problem)
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
   Example problems:
    - simpleEightPuzzle, mediumEightPuzzle, complexEghtPuzzle
    - slidingPuzzle 15 [ 1, 2, 3, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
    - queens 4, queens 8
    - simpleKnuthm, complexKnuth
    - simpleRouteFinding
    - simpleMotionPlanning
   (For the problems other than the sliding puzzle, we need to set `problemStateToHtml` to `Nothing`,
   because I have not yet coded visualizations of their states.)

   Search techniques (graph and tree search):
    - DepthFirst, TreeDepthFirst
    - BreadthFirst, TreeBreadthFirst
    - UniformCost, TreeUniformCost (=Dijkstra)
    - Greedy, TreeGreedy
    - BestFirst, TreeBestFirst

   Visuals:
    - Scatter (x-axis: depth, y-axis: heuristic)
    - Tree
    - TreeMap
    - Graph

   Documentation:
   https://package.elm-lang.org/packages/davidpomerenke/elm-problem-solving/latest/

   Github:
   https://github.com/davidpomerenke/elm-problem-solving

-}
--
--
--
-- Example implementation of a problem: Vacuum world.


type Location
    = A
    | B


type Condition
    = Clean
    | Dirty


type alias State =
    { location : Location
    , a : Condition
    , b : Condition
    }


type alias Action =
    State -> State


left : Action
left state =
    { state | location = A }


right : Action
right state =
    { state | location = B }


suck : Action
suck state =
    case state.location of
        A ->
            { state | a = Clean }

        B ->
            { state | b = Clean }


vacuumWorld : Problem State
vacuumWorld =
    { initialState =
        { location = A
        , a = Dirty
        , b = Dirty
        }
    , actions =
        \state ->
            [ { stepCost = 1, result = left state }
            , { stepCost = 1, result = right state }
            , { stepCost = 1, result = suck state }
            ]
    , heuristic = \_ -> 0
    , goalTest = \state -> state.a == Clean && state.b == Clean
    , stateToString = Debug.toString
    }



-- Learn Elm: https://elmprogramming.com/
-- Elm core documentation: https://package.elm-lang.org/packages/elm/core/latest/
