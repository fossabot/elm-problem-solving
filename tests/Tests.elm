module Tests exposing (..)

import Expect
import Problem.Search exposing (..)
import Problem exposing (..)
import Problem.Example.Graph exposing (..)
import Problem.Example.KnuthConjecture exposing (..)
import Problem.Example.MotionPlanning as MotionPlanning
import Problem.Example.SlidingPuzzle exposing (..)
import Problem.Example.Queens exposing (..)
import Problem.Example.VacuumWorld exposing (..)
import Test exposing (..)
 
  
suite : Test
suite =
    describe "elm-ai"
        [ describe "searching"
            [ describe "breadth-first search" bfs
            , describe "depth-first search" dfs
            , describe "uniform-cost search" ucs
            , describe "greedy search" cfs
            , describe "best-first search" ass
            ]
        ]



-- SEARCH


{-| Performs n search steps, and produces a list of snapshots of the states in the frontier after each step.
-}
scanFrontierStates :
    Model a
    -> Int
    -> List (List a)
    -> List (List a)
scanFrontierStates model n frontiers =
    if n > 0 then
        let
            nextModel =
                next model
        in
        scanFrontierStates nextModel (n - 1) (frontiers ++ [ nextModel.frontier ])

    else
        frontiers


solvesWithPath : Model a -> List ( Float, a ) -> Expect.Expectation
solvesWithPath searchModel path_ =
    Expect.equal
        (let
            ( solution, model ) =
                solve searchModel
         in
         Maybe.map (path model) solution
        )
        (Just (List.reverse path_)) 


solvesWithState searchModel visualize state =
    Expect.equal
        (let
            ( solution, _ ) =
                solve searchModel
         in
         Maybe.map (.state >> visualize) solution
        )
        (Just state)


{-| Shift string to the left by n spaces in each line.
-}
addSpaces n =
    String.split "\n" >> String.join ("\n" ++ String.repeat n " ")


bfs =
    [ -- expansion steps
      test "expands nodes correctly in route finding" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (breadthFirst (routeFinding identity "Arad" "Bucharest" romania.distance)) 21 [])
                [ [ "Timisoara", "Sibiu", "Zerind" ]
                , [ "Oradea", "Timisoara", "Sibiu" ]
                , [ "Rimnicu Vilcea", "Fagaras", "Oradea", "Timisoara" ]
                , [ "Lugoj", "Rimnicu Vilcea", "Fagaras", "Oradea" ]
                , [ "Lugoj", "Rimnicu Vilcea", "Fagaras" ]
                , [ "Bucharest", "Lugoj", "Rimnicu Vilcea" ]
                , [ "Craiova", "Pitesti", "Bucharest", "Lugoj" ]
                , [ "Mehadia", "Craiova", "Pitesti", "Bucharest" ]
                , [ "Giurgiu", "Urziceni", "Mehadia", "Craiova", "Pitesti" ]
                , [ "Giurgiu", "Urziceni", "Mehadia", "Craiova" ]
                , [ "Drobeta", "Giurgiu", "Urziceni", "Mehadia" ]
                , [ "Drobeta", "Giurgiu", "Urziceni" ]
                , [ "Hirsova", "Vaslui", "Drobeta", "Giurgiu" ]
                , [ "Hirsova", "Vaslui", "Drobeta" ]
                , [ "Hirsova", "Vaslui" ]
                , [ "Iasi", "Hirsova" ]
                , [ "Eforie", "Iasi" ]
                , [ "Neamt", "Eforie" ]
                , [ "Neamt" ]
                , []
                , []
                ]
    , test "expands nodes correctly in simple eight puzzle" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (breadthFirst simpleEightPuzzle) 1 [])
                [ [ [ 1, 4, 2, 3, 5, 0, 6, 7, 8 ]
                  , [ 1, 4, 2, 0, 3, 5, 6, 7, 8 ]
                  , [ 1, 4, 2, 3, 7, 5, 6, 0, 8 ]
                  , [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ]
                  ]
                ]
    , test "expands nodes correctly in simple knuth conjecture" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (breadthFirst simpleKnuthConjecture) 4 [])
                [ [ 24, 2 ]
                , [ 1.4142135623730951, 24 ]
                , [ 6.204484017332394e23, 4.898979485566356, 1.4142135623730951 ]
                , [ 1.189207115002721, 1, 6.204484017332394e23, 4.898979485566356 ]
                ]

    -- solving whole problems
    , test "solves vacuum world correctly" <|
        \_ ->
            solvesWithPath (breadthFirst vacuumWorld)
                [ ( 0, { a = Dirty, b = Dirty, location = A } )
                , ( 1, { a = Clean, b = Dirty, location = A } )
                , ( 2, { a = Clean, b = Dirty, location = B } )
                , ( 3, { a = Clean, b = Clean, location = B } ) 
                ]
    , test "finds route from arad to bucharest" <|
        \_ ->
            solvesWithPath
                (breadthFirst <| routeFinding identity "Arad" "Bucharest" romania.distance)
                [ ( 0, "Arad" )
                , ( 140, "Sibiu" )
                , ( 239, "Fagaras" )
                , ( 450, "Bucharest" )
                ]
    , test "solves simple eight puzzle correctly" <|
        \_ ->
            solvesWithPath (breadthFirst simpleEightPuzzle)
                [ ( 0, [ 1, 4, 2, 3, 0, 5, 6, 7, 8 ] )
                , ( 1, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                , ( 2, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                ]
    , test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (breadthFirst incrementalEightQueens)
                Problem.Example.Queens.visualize
                [ [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                , [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                ]
    , test "solves simple knuth conjecture correctly" <|
        \_ ->
            solvesWithPath (breadthFirst simpleKnuthConjecture)
                [ ( 0, 4 )
                , ( 1, 2 )
                , ( 2, 1.4142135623730951 )
                , ( 3, 1 )
                ]
    , test "solves simple motion planning problem" <|
        \_ ->
            Expect.equal
                (breadthFirst MotionPlanning.simpleProblem
                    |> solve 
                    |> Tuple.second
                    |> MotionPlanning.stateToString MotionPlanning.simpleConfig
                    |> addSpaces 16 
                )
                """
                ╭────────────────────╮
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅●|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅•⋅|
                |⋅⋅⋅⋅⋅▒▒▒▒▒▒▒▒▒▒▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅▒         ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅▒▒▒▒▒▒▒▒▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅•⋅⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒▒▒•⋅⋅⋅|
                |⋅⋅⋅⋅⋅⋅⋅•••••••••⋅⋅⋅⋅|
                |⋅⋅⋅⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |○⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                ╰────────────────────╯
                """
    ]


dfs =
    [ -- expansion steps
      test "expands nodes correctly in route finding" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (depthFirst (routeFinding identity "Arad" "Bucharest" romania.distance)) 21 [])
                [ [ "Timisoara", "Sibiu", "Zerind" ]
                , [ "Lugoj", "Sibiu", "Zerind" ]
                , [ "Mehadia", "Sibiu", "Zerind" ]
                , [ "Drobeta", "Sibiu", "Zerind" ]
                , [ "Craiova", "Sibiu", "Zerind" ]
                , [ "Rimnicu Vilcea", "Pitesti", "Sibiu", "Zerind" ]
                , [ "Pitesti", "Sibiu", "Zerind" ]
                , [ "Bucharest", "Sibiu", "Zerind" ]
                , [ "Giurgiu", "Urziceni", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Urziceni", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Hirsova", "Vaslui", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Eforie", "Vaslui", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Vaslui", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Iasi", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Neamt", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Fagaras", "Sibiu", "Zerind" ]
                , [ "Sibiu", "Zerind" ]
                , [ "Oradea", "Zerind" ]
                , [ "Zerind" ]
                , []
                , []
                ]

    -- solving whole problems
    , test "solves vacuum world correctly" <|
        \_ ->
            solvesWithPath (depthFirst vacuumWorld)
                [ ( 0, { a = Dirty, b = Dirty, location = A } )
                , ( 1, { a = Clean, b = Dirty, location = A } )
                , ( 2, { a = Clean, b = Dirty, location = B } )
                , ( 3, { a = Clean, b = Clean, location = B } )
                ]
    , test "finds route from arad to bucharest" <|
        \_ ->
            solvesWithPath
                (depthFirst <| routeFinding identity "Arad" "Bucharest" romania.distance)
                [ ( 0, "Arad" )
                , ( 118, "Timisoara" )
                , ( 229, "Lugoj" )
                , ( 299, "Mehadia" )
                , ( 374, "Drobeta" )
                , ( 494, "Craiova" )
                , ( 632, "Pitesti" )
                , ( 733, "Bucharest" )
                ]

    -- does not solve eight puzzle
    , test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (depthFirst incrementalEightQueens)
                Problem.Example.Queens.visualize
                [ [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                ]

    -- does not solve knuth conjecture
    , test "solves simple motion planning problem" <|
        \_ ->
            Expect.equal
                (depthFirst MotionPlanning.simpleProblem
                    |> solve
                    |> Tuple.second
                    |> MotionPlanning.stateToString MotionPlanning.simpleConfig
                    |> addSpaces 16
                )
                """
                ╭────────────────────╮
                |⋅••⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅••⋅●|
                |•⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅•⋅⋅•⋅|
                |⋅•⋅⋅•▒▒▒▒▒▒▒▒▒▒▒•⋅⋅⋅|
                |•⋅⋅•⋅▒         ▒⋅•⋅ |
                |⋅•⋅⋅•▒▒▒▒▒▒▒▒▒ ▒•⋅⋅ |
                |•⋅⋅•⋅⋅⋅••⋅⋅  ▒ ▒⋅•⋅ |
                |⋅•⋅⋅•⋅•⋅⋅•⋅⋅ ▒ ▒•⋅⋅ |
                |•⋅⋅•⋅⋅⋅•⋅⋅•⋅ ▒ ▒⋅•⋅⋅|
                |⋅•⋅⋅•⋅•⋅⋅•⋅⋅ ▒ ▒⋅⋅•⋅|
                |•⋅⋅•⋅⋅⋅•⋅⋅•⋅⋅▒▒▒⋅⋅⋅•|
                |⋅•⋅⋅•⋅•⋅⋅•⋅⋅•⋅•⋅•⋅•⋅|
                |•⋅⋅•⋅⋅⋅•⋅⋅••⋅•⋅•⋅•⋅⋅|
                |⋅•⋅⋅•⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |•⋅⋅•⋅⋅⋅•⋅•⋅•⋅•⋅•⋅•⋅⋅|
                |⋅•⋅⋅•⋅⋅⋅•⋅•⋅•⋅•⋅•⋅•⋅|
                |•⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅•|
                |⋅•⋅⋅•⋅•⋅•⋅•⋅•⋅•⋅•⋅•⋅|
                |○⋅⋅⋅⋅•⋅•⋅•⋅•⋅•⋅•⋅•⋅⋅|
                ╰────────────────────╯
                """
    ]


ucs =
    [ -- expansion steps
      test "expands nodes correctly in route finding" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (uniformCost (routeFinding identity "Arad" "Bucharest" romania.distance)) 21 [])
                [ [ "Timisoara", "Sibiu", "Zerind" ]
                , [ "Oradea", "Timisoara", "Sibiu" ]
                , [ "Lugoj", "Oradea", "Sibiu" ]
                , [ "Rimnicu Vilcea", "Fagaras", "Lugoj", "Oradea" ]
                , [ "Rimnicu Vilcea", "Fagaras", "Lugoj" ]
                , [ "Craiova", "Pitesti", "Fagaras", "Lugoj" ]
                , [ "Mehadia", "Craiova", "Pitesti", "Fagaras" ]
                , [ "Bucharest", "Mehadia", "Craiova", "Pitesti" ]
                , [ "Drobeta", "Bucharest", "Craiova", "Pitesti" ]
                , [ "Drobeta", "Bucharest", "Craiova" ]
                , [ "Drobeta", "Bucharest" ]
                , [ "Bucharest" ]
                , [ "Giurgiu", "Urziceni" ]
                , [ "Hirsova", "Vaslui", "Giurgiu" ]
                , [ "Hirsova", "Vaslui" ]
                , [ "Eforie", "Vaslui" ]
                , [ "Iasi", "Eforie" ]
                , [ "Iasi" ]
                , [ "Neamt" ]
                , []
                , []
                ]
    , test "expands nodes correctly in simple knuth conjecture" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (uniformCost simpleKnuthConjecture) 4 [])
                [ [ 24, 2 ]
                , [ 6.204484017332394e23, 4.898979485566356, 2 ]
                , [ 1.4142135623730951, 6.204484017332394e23, 4.898979485566356 ]
                , [ 1.189207115002721, 1, 6.204484017332394e23, 4.898979485566356 ]
                ]

    -- solving whole problems
    , test "solves vacuum world correctly" <|
        \_ ->
            solvesWithPath (uniformCost vacuumWorld)
                [ ( 0, { a = Dirty, b = Dirty, location = A } )
                , ( 1, { a = Clean, b = Dirty, location = A } )
                , ( 2, { a = Clean, b = Dirty, location = B } )
                , ( 3, { a = Clean, b = Clean, location = B } )
                ]
    , test "finds route from arad to bucharest" <|
        \_ ->
            solvesWithPath
                (uniformCost <| routeFinding identity "Arad" "Bucharest" romania.distance)
                [ ( 0, "Arad" )
                , ( 140, "Sibiu" )
                , ( 239, "Fagaras" )
                , ( 450, "Bucharest" )
                ]
    , test "solves simple eight puzzle correctly" <|
        \_ ->
            solvesWithPath (uniformCost simpleEightPuzzle)
                [ ( 0, [ 1, 4, 2, 3, 0, 5, 6, 7, 8 ] )
                , ( 1, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                , ( 2, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                ]
    , test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (uniformCost incrementalEightQueens)
                Problem.Example.Queens.visualize
                [ [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                ]
    , test "solves simple knuth conjecture correctly" <|
        \_ ->
            solvesWithPath (uniformCost simpleKnuthConjecture)
                [ ( 0, 4 )
                , ( 1, 2 )
                , ( 2, 1.4142135623730951 )
                , ( 3, 1 )
                ]
    , test "solves simple motion planning problem" <|
        \_ ->
            Expect.equal
                (uniformCost MotionPlanning.simpleProblem
                    |> solve
                    |> Tuple.second
                    |> MotionPlanning.stateToString MotionPlanning.simpleConfig
                    |> addSpaces 16
                )
                """
                ╭────────────────────╮
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅●|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅•⋅|
                |⋅⋅⋅⋅⋅▒▒▒▒▒▒▒▒▒▒▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅▒         ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅▒▒▒▒▒▒▒▒▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅•⋅⋅|
                |⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅▒▒▒•⋅⋅⋅|
                |⋅⋅⋅⋅⋅⋅⋅•••••••••⋅⋅⋅⋅|
                |⋅⋅⋅⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                |○⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅|
                ╰────────────────────╯
                """
    ]


cfs =
    [ -- expansion steps
      test "expands nodes correctly in route finding" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (greedy (routeFinding identity "Arad" "Bucharest" distance)) 21 [])
                [ [ "Timisoara", "Sibiu", "Zerind" ]
                , [ "Lugoj", "Sibiu", "Zerind" ]
                , [ "Mehadia", "Sibiu", "Zerind" ]
                , [ "Drobeta", "Sibiu", "Zerind" ]
                , [ "Craiova", "Sibiu", "Zerind" ]
                , [ "Rimnicu Vilcea", "Pitesti", "Sibiu", "Zerind" ]
                , [ "Pitesti", "Sibiu", "Zerind" ]
                , [ "Bucharest", "Sibiu", "Zerind" ]
                , [ "Giurgiu", "Urziceni", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Urziceni", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Hirsova", "Vaslui", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Eforie", "Vaslui", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Vaslui", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Iasi", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Neamt", "Fagaras", "Sibiu", "Zerind" ]
                , [ "Fagaras", "Sibiu", "Zerind" ]
                , [ "Sibiu", "Zerind" ]
                , [ "Oradea", "Zerind" ]
                , [ "Zerind" ]
                , []
                , []
                ]

    -- solving whole problems
    , test "solves vacuum world correctly" <|
        \_ ->
            solvesWithPath (greedy vacuumWorld)
                [ ( 0, { a = Dirty, b = Dirty, location = A } )
                , ( 1, { a = Clean, b = Dirty, location = A } )
                , ( 2, { a = Clean, b = Dirty, location = B } )
                , ( 3, { a = Clean, b = Clean, location = B } )
                ]
    , test "finds route from arad to bucharest" <|
        \_ ->
            solvesWithPath
                (greedy <| routeFinding identity "Arad" "Bucharest" distance)
                [ ( 0, "Arad" )
                , ( 118, "Timisoara" )
                , ( 229, "Lugoj" )
                , ( 299, "Mehadia" )
                , ( 374, "Drobeta" )
                , ( 494, "Craiova" )
                , ( 632, "Pitesti" )
                , ( 733, "Bucharest" )
                ]
    , test "solves simple eight puzzle correctly" <|
        \_ ->
            solvesWithPath (greedy simpleEightPuzzle)
                [ ( 0, [ 1, 4, 2, 3, 0, 5, 6, 7, 8 ] )
                , ( 1, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                , ( 2, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                ]
    , test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (greedy incrementalEightQueens)
                Problem.Example.Queens.visualize
                [ [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                ]

    -- does not solve knuth conjecture
    , test "solves simple motion planning problem" <|
        \_ ->
            Expect.equal
                (greedy MotionPlanning.simpleProblem
                    |> solve
                    |> Tuple.second
                    |> MotionPlanning.stateToString MotionPlanning.simpleConfig
                    |> addSpaces 16
                )
                """
                ╭────────────────────╮
                |                  ⋅●|
                |                  ⋅•|
                |     ▒▒▒▒▒▒▒▒▒▒▒  ⋅•|
                |     ▒         ▒  ⋅•|
                |     ▒▒▒▒▒▒▒▒▒ ▒  ⋅•|
                |       ⋅⋅⋅⋅⋅⋅▒ ▒  ⋅•|
                |       ⋅⋅⋅⋅⋅⋅▒ ▒ ⋅⋅•|
                |        ⋅⋅••⋅▒ ▒⋅⋅•⋅|
                |       ⋅⋅•⋅⋅•▒ ▒⋅•⋅⋅|
                |      ⋅⋅•⋅⋅⋅•▒▒▒•⋅⋅ |
                |     ⋅⋅•⋅⋅ ⋅⋅•••⋅⋅  |
                |    ⋅⋅•⋅⋅   ⋅⋅⋅⋅⋅   |
                |   ⋅⋅•⋅⋅            |
                |  ⋅⋅•⋅⋅             |
                | ⋅⋅•⋅⋅              |
                |⋅⋅•⋅⋅               |
                |⋅•⋅⋅                |
                |○⋅⋅                 |
                ╰────────────────────╯
                """
    ]


ass =
    [ -- expansion steps
      test "expands nodes correctly in route finding" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (bestFirst (routeFinding identity "Arad" "Bucharest" distance)) 21 [])
                [ [ "Timisoara", "Sibiu", "Zerind" ]
                , [ "Oradea", "Timisoara", "Sibiu" ]
                , [ "Lugoj", "Oradea", "Sibiu" ]
                , [ "Rimnicu Vilcea", "Fagaras", "Lugoj", "Oradea" ]
                , [ "Rimnicu Vilcea", "Fagaras", "Lugoj" ]
                , [ "Craiova", "Pitesti", "Fagaras", "Lugoj" ]
                , [ "Mehadia", "Craiova", "Pitesti", "Fagaras" ]
                , [ "Bucharest", "Mehadia", "Craiova", "Pitesti" ]
                , [ "Drobeta", "Bucharest", "Craiova", "Pitesti" ]
                , [ "Drobeta", "Bucharest", "Craiova" ]
                , [ "Drobeta", "Bucharest" ]
                , [ "Bucharest" ]
                , [ "Giurgiu", "Urziceni" ]
                , [ "Hirsova", "Vaslui", "Giurgiu" ]
                , [ "Hirsova", "Vaslui" ]
                , [ "Eforie", "Vaslui" ]
                , [ "Iasi", "Eforie" ]
                , [ "Iasi" ]
                , [ "Neamt" ]
                , []
                , []
                ]
    , test "expands nodes correctly in simple knuth conjecture" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (bestFirst simpleKnuthConjecture) 4 [])
                [ [ 24, 2 ]
                , [ 6.204484017332394e23, 4.898979485566356, 2 ]
                , [ 1.4142135623730951, 6.204484017332394e23, 4.898979485566356 ]
                , [ 1.189207115002721, 1, 6.204484017332394e23, 4.898979485566356 ]
                ]

    -- solving whole problems
    , test "solves vacuum world correctly" <|
        \_ ->
            solvesWithPath (bestFirst vacuumWorld)
                [ ( 0, { a = Dirty, b = Dirty, location = A } )
                , ( 1, { a = Clean, b = Dirty, location = A } )
                , ( 2, { a = Clean, b = Dirty, location = B } )
                , ( 3, { a = Clean, b = Clean, location = B } )
                ]
    , test "finds route from arad to bucharest" <|
        \_ ->
            solvesWithPath
                (bestFirst <| routeFinding identity "Arad" "Bucharest" distance)
                [ ( 0, "Arad" )
                , ( 140, "Sibiu" )
                , ( 239, "Fagaras" )
                , ( 450, "Bucharest" )
                ]
    , test "solves simple eight puzzle correctly" <|
        \_ ->
            solvesWithPath (bestFirst simpleEightPuzzle)
                [ ( 0, [ 1, 4, 2, 3, 0, 5, 6, 7, 8 ] )
                , ( 1, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                , ( 2, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                ]
    , test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (bestFirst incrementalEightQueens)
                Problem.Example.Queens.visualize
                [ [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                ]
    , test "solves simple knuth conjecture correctly" <|
        \_ ->
            solvesWithPath (bestFirst simpleKnuthConjecture)
                [ ( 0, 4 )
                , ( 1, 2 )
                , ( 2, 1.4142135623730951 )
                , ( 3, 1 )
                ]

    -- this works, just takes 30 seconds or so, so will not be included in tests
    -- testComplexEightPuzzle bestFirst
    {--, skip <|
        test "solves complex eight puzzle" <|
            \_ ->
                solvesWithPath
                    (bestFirst complexEightPuzzle)
                    [ ( 0, [ 7, 2, 4, 5, 0, 6, 8, 3, 1 ] )
                    , ( 1, [ 7, 2, 4, 0, 5, 6, 8, 3, 1 ] )
                    , ( 2, [ 0, 2, 4, 7, 5, 6, 8, 3, 1 ] )
                    , ( 3, [ 2, 0, 4, 7, 5, 6, 8, 3, 1 ] )
                    , ( 4, [ 2, 5, 4, 7, 0, 6, 8, 3, 1 ] )
                    , ( 5, [ 2, 5, 4, 7, 6, 0, 8, 3, 1 ] )
                    , ( 6, [ 2, 5, 4, 7, 6, 1, 8, 3, 0 ] )
                    , ( 7, [ 2, 5, 4, 7, 6, 1, 8, 0, 3 ] )
                    , ( 8, [ 2, 5, 4, 7, 6, 1, 0, 8, 3 ] )
                    , ( 9, [ 2, 5, 4, 0, 6, 1, 7, 8, 3 ] )
                    , ( 10, [ 2, 5, 4, 6, 0, 1, 7, 8, 3 ] )
                    , ( 11, [ 2, 5, 4, 6, 1, 0, 7, 8, 3 ] )
                    , ( 12, [ 2, 5, 4, 6, 1, 3, 7, 8, 0 ] )
                    , ( 13, [ 2, 5, 4, 6, 1, 3, 7, 0, 8 ] )
                    , ( 14, [ 2, 5, 4, 6, 1, 3, 0, 7, 8 ] )
                    , ( 15, [ 2, 5, 4, 0, 1, 3, 6, 7, 8 ] )
                    , ( 16, [ 2, 5, 4, 1, 0, 3, 6, 7, 8 ] )
                    , ( 17, [ 2, 5, 4, 1, 3, 0, 6, 7, 8 ] )
                    , ( 18, [ 2, 5, 0, 1, 3, 4, 6, 7, 8 ] )
                    , ( 19, [ 2, 0, 5, 1, 3, 4, 6, 7, 8 ] )
                    , ( 20, [ 0, 2, 5, 1, 3, 4, 6, 7, 8 ] )
                    , ( 21, [ 1, 2, 5, 0, 3, 4, 6, 7, 8 ] )
                    , ( 22, [ 1, 2, 5, 3, 0, 4, 6, 7, 8 ] )
                    , ( 23, [ 1, 2, 5, 3, 4, 0, 6, 7, 8 ] )
                    , ( 24, [ 1, 2, 0, 3, 4, 5, 6, 7, 8 ] )
                    , ( 25, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                    , ( 26, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                    ]
    --}
    , test "solves simple motion planning problem" <|
        \_ ->
            Expect.equal
                (bestFirst MotionPlanning.simpleProblem
                    |> solve
                    |> Tuple.second
                    |> MotionPlanning.stateToString MotionPlanning.simpleConfig
                    |> addSpaces 16
                )
                """
                ╭────────────────────╮
                |                  ⋅●|
                |                  ⋅•|
                |     ▒▒▒▒▒▒▒▒▒▒▒  ⋅•|
                |     ▒         ▒  ⋅•|
                |     ▒▒▒▒▒▒▒▒▒ ▒  ⋅•|
                |      ⋅⋅⋅⋅⋅⋅⋅▒ ▒  ⋅•|
                |      ⋅⋅⋅⋅⋅⋅⋅▒ ▒ ⋅⋅•|
                |      ⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅⋅•⋅|
                |      ⋅⋅⋅⋅⋅⋅⋅▒ ▒⋅•⋅⋅|
                |      ⋅⋅⋅⋅⋅⋅⋅▒▒▒•⋅⋅ |
                |     ⋅⋅•••••••••⋅⋅  |
                |    ⋅⋅•⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅   |
                |   ⋅⋅•⋅⋅            |
                |  ⋅⋅•⋅⋅             |
                | ⋅⋅•⋅⋅              |
                |⋅⋅•⋅⋅               |
                |⋅•⋅⋅                |
                |○⋅⋅                 |
                ╰────────────────────╯
                """
    ]
