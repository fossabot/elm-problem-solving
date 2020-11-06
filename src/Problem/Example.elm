module Problem.Example exposing
    ( VacuumWorld, vacuumWorld
    , SlidingPuzzle, slidingPuzzle, simpleEightPuzzle, mediumEightPuzzle, complexEightPuzzle, slidingPuzzleVisual
    , Queens, queens
    , Knuth, knuth, simpleKnuth, complexKnuth
    , Graph, romania, routeFinding, simpleRouteFinding
    , MotionPlanning, motionPlanning, simpleMotionPlanning
    )

{-| Examples


# Toy Problems


## Vacuum World

@docs VacuumWorld, vacuumWorld


## Sliding Puzzle

@docs SlidingPuzzle, slidingPuzzle, simpleEightPuzzle, mediumEightPuzzle, complexEightPuzzle, slidingPuzzleVisual


## N-Queens Problem

@docs Queens, queens


## Knuth Conjecture

@docs Knuth, knuth, simpleKnuth, complexKnuth


# Real-world problems


## Graph search

In order to solve any problem, we use search techniques that create a graph. But we can also use these search techniques to search not an abstract problem, but a concrete graph.

@docs Graph, romania, routeFinding, simpleRouteFinding


## Motion planning

@docs MotionPlanning, motionPlanning, simpleMotionPlanning

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Problem exposing (Problem)
import Problem.Example.Graph exposing (bucharestDistance, straightLineDistance)
import Problem.Example.KnuthConjecture exposing (simpleKnuthConjecture)
import Problem.Example.MotionPlanning
import Problem.Example.Queens
import Problem.Example.SlidingPuzzle
import Problem.Example.VacuumWorld


{-| -}
type alias VacuumWorld =
    Problem.Example.VacuumWorld.State


{-| This is the simplest example problem: The world of a vacuum cleaner.

Let's have a complete look at how it works internally:

There are two locations, A and B, and each of them can be either clean or dirty.

    type alias State =
        { location : Location
        , a : Condition
        , b : Condition
        }

    type Location
        = A
        | B

    type Condition
        = Clean
        | Dirty


### `initialState`

At the start, both are dirty:

    initialState =
        { location = A
        , a = Dirty
        , b = Dirty
        }


### `actions`

There are three actions: Moving left, moving right, and sucking:

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

We also need to specify a step cost for each action. Let's just set it at 1 for every action. We can add the step cost to all actions like this:

    actions =
        \state ->
            List.map
                (\action -> { stepCost = 1, result = action state })
                [ left, right, suck ]


### `heuristic`

We could provide a heuristic for each state. This can speed up the search algorithms a lot when we are dealing with complex problems. Our present problem is not very complex, so we just set it at 0 for every state:

    heuristic =
        \_ -> 0


### `goalTest`

The objective of our vacuum cleaner robot is to clean both locations:

    goalTest =
        \state -> state.a == Clean && state.b == Clean

-}
vacuumWorld : Problem VacuumWorld
vacuumWorld =
    Problem.Example.VacuumWorld.vacuumWorld


{-| -}
type alias SlidingPuzzle =
    Problem.Example.SlidingPuzzle.State


{-| Sliding Puzzle

A quadratic grid with n²-1 numbers. They are in a disturbed order and should be brought back to their natural order, by only moving around the tile with number 0. A (4²-1)-puzzle = 15-puzzle looks like this:

![15-Puzzle](15puzzle.png)

A sliding puzzle is represented internally as a simple list. The positions where a new line begins are determined by the square root of the length of the list.

    type alias State =
        List Int

It could be represented as a list of lists of equal lengths alternatively. (Then it is harder to deal with the equal-length assumption, though.)

-}
slidingPuzzle : SlidingPuzzle -> Problem SlidingPuzzle
slidingPuzzle =
    Problem.Example.SlidingPuzzle.slidingPuzzle


{-|

    simpleEightPuzzle =
        slidingPuzzle <|
            concat
                [ [ 1, 4, 2 ]
                , [ 3, 0, 5 ]
                , [ 6, 7, 8 ]
                ]

-}
simpleEightPuzzle : Problem SlidingPuzzle
simpleEightPuzzle =
    Problem.Example.SlidingPuzzle.simpleEightPuzzle


{-|

    mediumEightPuzzle =
        slidingPuzzle <|
            concat
                [ [ 1, 4, 2 ]
                , [ 3, 5, 8 ]
                , [ 0, 6, 7 ]
                ]

-}
mediumEightPuzzle : Problem SlidingPuzzle
mediumEightPuzzle =
    Problem.Example.SlidingPuzzle.mediumEightPuzzle


{-| Complicated eight puzzle.

    complexEightPuzzle =
        slidingPuzzle <|
            concat
                [ [ 7, 2, 4 ]
                , [ 5, 0, 6 ]
                , [ 8, 3, 1 ]
                ]

-}
complexEightPuzzle : Problem SlidingPuzzle
complexEightPuzzle =
    Problem.Example.SlidingPuzzle.complexEightPuzzle


{-| -}
slidingPuzzleVisual : SlidingPuzzle -> Html msg
slidingPuzzleVisual =
    Problem.Example.SlidingPuzzle.visualize


{-| -}
type alias Queens =
    Problem.Example.Queens.State


{-| On an n times n chess board, place 8 queens without any queen attacking another.

    type alias State =
        List ( Int, Int )

-}
queens : Int -> Problem Queens
queens =
    Problem.Example.Queens.incrementalNQueens


{-| -}
type alias Knuth =
    Problem.Example.KnuthConjecture.State


{-| Knuth conjecture.

This is an artificial problem devised by Donald Knuth. (Donald Knuth is the person who is writing the book with all the algorithms in it, of which, if you understand it completely, you have high chances of getting a nice job.)

The idea is that every number can be represented by starting from 4 and then repeatedly applying one of these three functions:

  - Factorial!
  - √Square Root
  - ⌊Floor operation⌋ (that is, rounding downwards)

We want to find the sequence of concatenated functions to arrive at a given number.

Internally, this looks like so:

    type alias State =
        Float

    actions =
        \n ->
            if toFloat (round n) == n then
                [ { stepCost = 1, result = sqrt n }
                , { stepCost = 1, result = toFloat (factorial (round n)) }
                ]

            else
                [ { stepCost = 1, result = toFloat (floor n) }
                , { stepCost = 1, result = sqrt n }
                ]

-}
knuth : Float -> Problem Knuth
knuth =
    Problem.Example.KnuthConjecture.knuthConjecture


{-|

    simpleKnuth =
        knuth 1

-}
simpleKnuth : Problem Knuth
simpleKnuth =
    knuth 1


{-|

    complexKnuth =
        knuth 5

-}
complexKnuth : Problem Knuth
complexKnuth =
    knuth 5


{-| A graph is represented as a dictionary for our purposes. The keys are the start nodes, and the values are lists of all the reachable end nodes and the distance between the start and end node.
-}
type alias Graph comparable =
    Dict comparable (List { stepCost : Float, result : comparable })


{-| Map of cities in Romania. Contains a graph of the distances between all connected cities, and a graph of the straight line distance.

![Romanian cities.](Romania.png)

(Unfortunately, we only have data for the straight line distance between Bucharest and the other cities, and not mutually between the other cities; so we call it `bucharestDistance` instead of `straightLineDistance`.)

-}
romania :
    { distance : Graph String
    , bucharestDistance : String -> Float
    }
romania =
    Problem.Example.Graph.romania


{-| Find a route of nodes in a graph.

The implementation is very straightforward:

    routeFinding nodeToString root goal graph =
        { initialState = root
        , actions = \a -> Dict.get a graph |> Maybe.withDefault []
        , heuristic = \_ -> 0
        , goalTest = (==) goal
        , stateToString = nodeToString
        }

Heuristics are not yet supported, unfortunately. (The reason is that I am not sure how best to deal with heuristics that are only defined for a subset of nodes, such as `romania.bucharestDistance`.)

-}
routeFinding : (comparable -> String) -> comparable -> comparable -> Graph comparable -> Problem comparable
routeFinding =
    Problem.Example.Graph.routeFinding


{-| Find a route from Arad to Bucharest.

    simpleRouteFinding =
        routeFinding identity "Arad" "Bucharest" romania.distance

-}
simpleRouteFinding : Problem String
simpleRouteFinding =
    Problem.Example.Graph.simpleRouteFinding


{-| -}
type alias MotionPlanning =
    Problem.Example.MotionPlanning.State


{-| Planning a movement on a rectangular grid. (Such as they occur in old computer games, for example.)

    type alias Config =
        { size : Position
        , obstacles : List Obstacle
        , start : Position
        , goal : Position
        }

    type alias Obstacle =
        List Position

    type alias Position =
        ( Int, Int )

[On improving grid representations.](https://www.redblobgames.com/pathfinding/grids/algorithms.html)

-}
motionPlanning : Problem.Example.MotionPlanning.Config -> Problem MotionPlanning
motionPlanning =
    Problem.Example.MotionPlanning.motionPlanning


{-| This example is inspired by an animation by user Subh83 [on Wikimedia](https://commons.wikimedia.org/wiki/File:Astar_progress_animation.gif). The animation is licensed under a Creative Commons Attribution 3.0 Unported license. It shows the problem configuration, and how the problem is solved by using A\* search.

![Motion planning visualization](https://commons.wikimedia.org/wiki/File:Astar_progress_animation.gif)

-}
simpleMotionPlanning : Problem MotionPlanning
simpleMotionPlanning =
    Problem.Example.MotionPlanning.simpleProblem
