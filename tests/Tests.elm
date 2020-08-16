module Tests exposing (..)

import Expect
import Games exposing (..)
import Games.TicTacToe exposing (..)
import Search exposing (..)
import Search.Problem exposing (..)
import Search.Problem.Graph exposing (routeFinding)
import Search.Problem.KnuthConjecture exposing (..)
import Search.Problem.NPuzzle exposing (..)
import Search.Problem.NQueens exposing (..)
import Search.Problem.Romania exposing (distance)
import Search.Problem.VacuumWorld exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "elm-ai"
        [ describe "searching"
            [ describe "breadth-first search" bfs
            , skip <| describe "depth-first search" dfs
            , skip <| describe "uniform-cost search" ucs
            , skip <| describe "best-first search" ass
            ]
        , skip <|
            describe "games"
                [ describe "maximin" maximin ]
        ]



-- SEARCH


{-| Performs n search steps, and produces a list of snapshots of the states in the frontier after each step.
-}
scanFrontierStates :
    Search.Model a comparable
    -> Int
    -> List (List a)
    -> List (List a)
scanFrontierStates model n frontiers =
    if n > 0 then
        let
            nextModel =
                next model
        in
        scanFrontierStates nextModel (n - 1) (frontiers ++ [ List.map .state nextModel.frontier ])

    else
        frontiers


solvesWithPath searchModel path_ =
    Expect.equal
        (searchModel
            |> nextGoal
            |> Tuple.first
            |> Maybe.map path
        )
        (Just (List.reverse path_))


solvesWithState searchModel visualize state =
    Expect.equal
        (searchModel
            |> nextGoal
            |> Tuple.first
            |> Maybe.map (\node -> visualize node.state)
        )
        (Just state)


bfs =
    [ -- expansion steps
      test "expands nodes correctly in route finding" <|
        \_ ->
            Expect.equal
                (scanFrontierStates (breadthFirst (routeFinding "Arad" "Bucharest" distance)) 21 [])
                [ -- expand arad
                  [ "Timisoara", "Sibiu", "Zerind" ]

                -- expand zerind
                , [ "Oradea", "Timisoara", "Sibiu" ]

                -- expand sibiu
                , [ "Rimnicu Vilcea", "Fagaras", "Oradea", "Timisoara" ]

                -- expand timisoara
                , [ "Lugoj", "Rimnicu Vilcea", "Fagaras", "Oradea" ]

                -- expand oradea
                , [ "Lugoj", "Rimnicu Vilcea", "Fagaras" ]

                -- expand fagaras
                , [ "Bucharest", "Lugoj", "Rimnicu Vilcea" ]

                -- expand rimnicu vilcea
                , [ "Craiova", "Pitesti", "Bucharest", "Lugoj" ]

                -- expand lugoj
                , [ "Mehadia", "Craiova", "Pitesti", "Bucharest" ]

                -- expand bucharest
                , [ "Giurgiu", "Urziceni", "Mehadia", "Craiova", "Pitesti" ]

                -- expand Pitesti
                , [ "Giurgiu", "Urziceni", "Mehadia", "Craiova" ]

                -- expand craiova
                , [ "Drobeta", "Giurgiu", "Urziceni", "Mehadia" ]

                -- expand mehadia
                , [ "Drobeta", "Giurgiu", "Urziceni" ]

                -- expand urziceni
                , [ "Hirsova", "Vaslui", "Drobeta", "Giurgiu" ]

                -- expand giurgiu
                , [ "Hirsova", "Vaslui", "Drobeta" ]

                -- expand drobeta
                , [ "Hirsova", "Vaslui" ]

                -- expand vaslui
                , [ "Iasi", "Hirsova" ]

                -- expand hirsova
                , [ "Eforie", "Iasi" ]

                -- expand iasi
                , [ "Neamt", "Eforie" ]

                -- expand eforie
                , [ "Neamt" ]

                -- expand neamt
                , []

                -- expand empty
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
    {- , skip <|
       test "solves vacuum world correctly" <|
           \_ ->
               solvesWithPath (breadthFirst vacuumWorld)
                   []
    -}
    , test "finds route from arad to bucharest" <|
        \_ ->
            solvesWithPath
                (breadthFirst <| routeFinding "Arad" "Bucharest" distance)
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
                Search.Problem.NQueens.visualize
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
    ]


dfs =
    [ -- expansion steps
      test "expands first node correctly in simple eight puzzle" <|
        \_ ->
            Expect.equal
                (List.map .state (next (depthFirst simpleEightPuzzle)).frontier)
                [ [ 1, 4, 2, 3, 5, 0, 6, 7, 8 ]
                , [ 1, 4, 2, 0, 3, 5, 6, 7, 8 ]
                , [ 1, 4, 2, 3, 7, 5, 6, 0, 8 ]
                , [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ]
                ]

    -- solving whole problems
    , test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (depthFirst incrementalEightQueens)
                Search.Problem.NQueens.visualize
                [ [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                ]
    ]


ucs =
    [ --, testSimpleEightPuzzle uniformCost
      test "solves incremental eight queens correctly" <|
        \_ ->
            solvesWithState
                (breadthFirst incrementalEightQueens)
                Search.Problem.NQueens.visualize
                [ [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                ]
    ]


ass =
    [-- testSimpleEightPuzzle uniformCost
     -- this works, just takes 30 seconds or so, so will not be included in tests
     -- testComplexEightPuzzle bestFirst
    ]


testComplexEightPuzzle s =
    test "solves complex eight puzzle" <|
        \_ ->
            Expect.equal
                (complexEightPuzzle
                    |> s
                    |> nextGoal
                    |> Tuple.first
                    |> Maybe.map path
                )
                (Just
                    [ ( 0, [ 7, 2, 4, 5, 0, 6, 8, 3, 1 ] )
                    , ( 1, [ 7, 2, 4, 5, 3, 6, 8, 0, 1 ] )
                    , ( 2, [ 7, 2, 4, 5, 3, 6, 0, 8, 1 ] )
                    , ( 3, [ 7, 2, 4, 5, 3, 0, 6, 8, 1 ] )
                    , ( 4, [ 7, 2, 4, 5, 3, 1, 6, 8, 0 ] )
                    , ( 5, [ 7, 2, 4, 5, 3, 1, 6, 0, 8 ] )
                    , ( 6, [ 7, 2, 4, 5, 3, 1, 0, 6, 8 ] )
                    , ( 7, [ 7, 2, 4, 0, 3, 1, 5, 6, 8 ] )
                    , ( 8, [ 7, 2, 0, 4, 3, 1, 5, 6, 8 ] )
                    , ( 9, [ 7, 0, 2, 4, 3, 1, 5, 6, 8 ] )
                    , ( 10, [ 7, 3, 2, 4, 0, 1, 5, 6, 8 ] )
                    , ( 11, [ 7, 3, 2, 4, 1, 0, 5, 6, 8 ] )
                    , ( 12, [ 7, 3, 2, 4, 1, 5, 0, 6, 8 ] )
                    , ( 13, [ 7, 3, 2, 0, 1, 5, 4, 6, 8 ] )
                    , ( 14, [ 0, 3, 2, 7, 1, 5, 4, 6, 8 ] )
                    , ( 15, [ 3, 0, 2, 7, 1, 5, 4, 6, 8 ] )
                    , ( 16, [ 3, 1, 2, 7, 0, 5, 4, 6, 8 ] )
                    , ( 17, [ 3, 1, 2, 0, 7, 5, 4, 6, 8 ] )
                    , ( 18, [ 3, 1, 2, 4, 7, 5, 0, 6, 8 ] )
                    , ( 19, [ 3, 1, 2, 4, 7, 5, 6, 0, 8 ] )
                    , ( 20, [ 3, 1, 2, 4, 0, 5, 6, 7, 8 ] )
                    , ( 21, [ 3, 1, 2, 0, 4, 5, 6, 7, 8 ] )
                    , ( 22, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                    ]
                )



-- GAMES


maximin =
    [ test "fills in, recognizes utility 0" <|
        \_ ->
            Expect.equal
                (minimaxDecision ticTacToe
                    [ [ 'x', 'x', 'o' ]
                    , [ 'o', 'o', 'x' ]
                    , [ 'x', 'o', ' ' ]
                    ]
                    Nothing
                )
                (Just
                    ( 0
                    , [ [ 'x', 'x', 'o' ]
                      , [ 'o', 'o', 'x' ]
                      , [ 'x', 'o', 'x' ]
                      ]
                    )
                )
    , test "fills in, recognizes utility 1" <|
        \_ ->
            Expect.equal
                (minimaxDecision ticTacToe
                    [ [ 'x', 'x', ' ' ]
                    , [ 'o', 'o', ' ' ]
                    , [ ' ', ' ', ' ' ]
                    ]
                    Nothing
                )
                (Just
                    ( 1
                    , [ [ 'x', 'x', 'x' ]
                      , [ 'o', 'o', ' ' ]
                      , [ ' ', ' ', ' ' ]
                      ]
                    )
                )
    , test "fills in, recognizes utility -1" <|
        \_ ->
            Expect.equal
                (minimaxDecision ticTacToe
                    [ [ 'x', 'x', 'o' ]
                    , [ 'o', 'o', ' ' ]
                    , [ ' ', ' ', 'x' ]
                    ]
                    Nothing
                )
                (Just
                    ( -1
                    , [ [ 'x', 'x', 'o' ]
                      , [ 'o', 'o', 'x' ]
                      , [ ' ', ' ', 'x' ]
                      ]
                    )
                )
    ]
