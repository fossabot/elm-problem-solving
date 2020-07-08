module Search.EightPuzzle exposing (..)

import Expect
import List exposing (all, concat, length, map, member, range)
import List.Extra exposing (elemIndex, swapAt)
import Maybe.Extra exposing (values)
import Search exposing (..)
import Test exposing (Test, test)


type alias State =
    List Int


vertical : Int -> State -> Maybe State
vertical d state =
    let
        free =
            elemIndex 0 state
    in
    Maybe.map (\a -> swapAt a (a + d) state) free


up s =
    vertical -s


down s =
    vertical s


horizontal : Int -> State -> Maybe State
horizontal d state =
    let
        free =
            elemIndex 0 state
    in
    Maybe.map (\a -> swapAt a (a + d) state) free


left =
    horizontal -1


right =
    horizontal 1


type EightPuzzleError
    = NotQuadratic
    | NoPermutationOfRange


type alias EightPuzzle =
    Result EightPuzzleError (SearchProblem State)


eightPuzzle : State -> EightPuzzle
eightPuzzle initialState =
    let
        side =
            sqrt (toFloat (length initialState))

        s =
            round side

        goal =
            range 0 (length initialState - 1)
    in
    if side /= toFloat s then
        Err NotQuadratic

    else if not (all (\a -> member a initialState) goal) then
        Err NoPermutationOfRange

    else
        Ok
            { initialState = initialState
            , actions =
                \state ->
                    [ up s, down s, left, right ]
                        |> map (\f -> f state)
                        |> values
                        |> map (\a -> ( 1, a ))
            , goalTest = \state -> state == goal
            }


simpleEightPuzzle : EightPuzzle
simpleEightPuzzle =
    eightPuzzle <|
        concat
            [ [ 1, 4, 2 ]
            , [ 3, 0, 5 ]
            , [ 6, 7, 8 ]
            ]


suite : () -> Test
suite =
    \_ ->
        test "Breadth-first search solves simple EightPuzzle" <|
            \_ ->
                Expect.equal
                    (simpleEightPuzzle
                        |> Result.map (\a -> a |> breadthFirstSearch |> Maybe.map path)
                    )
                    (Ok
                        (Just
                            [ ( 0, [ 1, 4, 2, 3, 0, 5, 6, 7, 8 ] )
                            , ( 1, [ 1, 0, 2, 3, 4, 5, 6, 7, 8 ] )
                            , ( 2, [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] )
                            ]
                        )
                    )
