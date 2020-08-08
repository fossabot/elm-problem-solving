module Search.Problem.IncrementalNQueens exposing (..)

import List.Extra as List
import Search exposing (Problem)


type alias State =
    List ( Int, Int )

incrementalEightQueens : Problem State
incrementalEightQueens = incrementalNQueens 8

incrementalNQueens : Int -> Problem State
incrementalNQueens n =
    { initialState = []
    , actions =
        \state ->
            let
                y =
                    List.length state
            in
            if y < n then
                List.range 0 (n - 1)
                    |> List.filter (\x -> not (isAttacked y x state))
                    |> List.map (\x -> ( 1, ( y, x ) :: state ))

            else
                []
    , heuristic = \_ -> 0
    , goalTest = \state -> List.length state == n
    }


isAttacked : Int -> Int -> State -> Bool
isAttacked y x state =
    List.any
        (\( yy, xx ) ->
            xx
                == x
                || yy
                == y
                || abs (yy - y)
                == abs (xx - x)
        )
        state


visualize : State -> List (List Int)
visualize state =
    List.map
        (\( y, x ) ->
            List.setAt x 1 (List.repeat (List.length state) 0)
        )
        state
