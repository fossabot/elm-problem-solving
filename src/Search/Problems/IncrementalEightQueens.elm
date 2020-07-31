module Search.Problems.IncrementalEightQueens exposing (..)

import Expect exposing (false)
import List.Extra as List
import Search exposing (Problem)


type alias State =
    List ( Int, Int )


positions : List Int
positions =
    [ 0, 1, 2, 3, 4, 5, 6, 7 ]


incrementalEightQueens : Problem State
incrementalEightQueens =
    { initialState = []
    , actions =
        \state ->
            let
                y =
                    List.length state
            in
            if y < 8 then
                positions
                    |> List.filter (\x -> not (isAttacked y x state))
                    |> List.map (\x -> ( 1, ( y, x ) :: state ))

            else
                []
    , goalTest = \state -> List.length state == 8
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
    List.map (\( y, x ) -> List.setAt x 1 [ 0, 0, 0, 0, 0, 0, 0, 0 ]) state
