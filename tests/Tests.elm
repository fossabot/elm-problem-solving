module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Loop
import Search exposing (..)
import Search.Problems.IncrementalNQueens exposing (..)
import Search.Problems.KnuthConjecture exposing (..)
import Search.Problems.NPuzzle exposing (..)
import Search.Problems.VacuumWorld exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "elm-ai"
        [ describe "searching"
            [ describe "breadth-first search"
                [ test "expands first node correctly in simple eight puzzle" <|
                    \_ ->
                        Expect.equal
                            (List.map .state (graphSearchStep (init insertLast simpleEightPuzzle)).frontier)
                            [ [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ]
                            , [ 1, 4, 2, 3, 7, 5, 6, 0, 8 ]
                            , [ 1, 4, 2, 0, 3, 5, 6, 7, 8 ]
                            , [ 1, 4, 2, 3, 5, 0, 6, 7, 8 ]
                            ]
                , test "solves simple eight puzzle" <|
                    \_ ->
                        Expect.equal
                            (Maybe.map path (Tuple.first (breadthFirstSearch simpleEightPuzzle)))
                            (Just
                                [ ( 0, [ 1, 4, 2, 3, 0, 5, 6, 7, 8 ] )
                                , ( 1, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                                , ( 2, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                                ]
                            )
                , test "solves incremental eight-queens problem" <|
                    \_ ->
                        Expect.equal
                            (Maybe.map (\node -> visualize node.state)
                                (Tuple.first (breadthFirstSearch incrementalEightQueens))
                            )
                            (Just
                                [ [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                                , [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                                ]
                            )
                , test "expands 1. node correctly in simple Knuth conjecture" <|
                    \_ ->
                        Expect.equal
                            (List.map .state (graphSearchStep (init insertLast simpleKnuthConjecture)).frontier)
                            [ 2, 24 ]
                , test "expands 2. node correctly in simple Knuth conjecture" <|
                    \_ ->
                        Expect.equal
                            (List.map .state (Loop.for 2 graphSearchStep (init insertLast simpleKnuthConjecture)).frontier)
                            [ 24, 1.4142135623730951 ]
                , test "expands 3. node correctly in simple Knuth conjecture" <|
                    \_ ->
                        Expect.equal
                            (List.map .state (Loop.for 3 graphSearchStep (init insertLast simpleKnuthConjecture)).frontier)
                            [ 1.4142135623730951, 4.898979485566356, 6.204484017332394e23 ]
                , test "expands 4. node correctly in simple Knuth conjecture" <|
                    \_ ->
                        Expect.equal
                            (List.map .state (Loop.for 4 graphSearchStep (init insertLast simpleKnuthConjecture)).frontier)
                            [ 4.898979485566356, 6.204484017332394e23 ]
                , test "solves simple Knuth conjecture" <|
                    \_ ->
                        Expect.equal
                            (Maybe.map path (Tuple.first (breadthFirstSearch simpleKnuthConjecture)))
                            (Just
                                [ ( 0, 4 )
                                , ( 1, 2 )
                                , ( 2, 1.4142135623730951 )
                                , ( 3, 1 )
                                ]
                            )
                ]
            , describe "depth-first search"
                [ test "expands first node correctly in simple eight puzzle" <|
                    \_ ->
                        Expect.equal
                            (List.map .state (graphSearchStep (init insertFirst simpleEightPuzzle)).frontier)
                            [ [ 1, 4, 2, 3, 5, 0, 6, 7, 8 ]
                            , [ 1, 4, 2, 0, 3, 5, 6, 7, 8 ]
                            , [ 1, 4, 2, 3, 7, 5, 6, 0, 8 ]
                            , [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ]
                            ]
                , test "solves incremental eight-queens problem" <|
                    \_ ->
                        Expect.equal
                            (Maybe.map (\node -> visualize node.state)
                                (Tuple.first (depthFirstSearch incrementalEightQueens))
                            )
                            (Just
                                [ [ 0, 0, 0, 0, 1, 0, 0, 0 ]
                                , [ 0, 0, 0, 0, 0, 0, 1, 0 ]
                                , [ 0, 1, 0, 0, 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0, 0, 1, 0, 0 ]
                                , [ 0, 0, 1, 0, 0, 0, 0, 0 ]
                                , [ 1, 0, 0, 0, 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 1, 0, 0, 0, 0 ]
                                , [ 0, 0, 0, 0, 0, 0, 0, 1 ]
                                ]
                            )
                ]
            ]
        ]
