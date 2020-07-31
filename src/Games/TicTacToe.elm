module Games.TicTacToe exposing (ticTacToe)

import Games exposing (Game)
import List.Extra as List
import Maybe.Extra as Maybe


type alias State =
    List Char


player : State -> Char
player state =
    if List.count ((==) 'A') state > List.count ((==) 'B') state then
        'B'

    else
        'A'


threeInARow : Char -> State -> Bool
threeInARow player_ state =
    List.any
        (\row ->
            List.all ((==) player_) row
        )
        (List.groupsOf 3 state)
        || List.any
            (\col ->
                List.all ((==) player_) col
            )
            (List.transpose (List.groupsOf 3 state))


ticTacToe : Game State
ticTacToe =
    { initialState = List.repeat 9 ' '
    , actions =
        \state ->
            List.range 0 8
                |> List.map
                    (\x ->
                        List.getAt x state
                            |> Maybe.map
                                (\p ->
                                    if p == ' ' then
                                        Just (List.setAt x (player state) state)

                                    else
                                        Nothing
                                )
                    )
                |> Maybe.values
                |> Maybe.values
                |> List.map (\a -> ( 1, a ))
    , terminalTest = \state -> threeInARow 'A' state || threeInARow 'B' state
    , utility =
        \state ->
            if threeInARow 'A' state then
                1

            else if threeInARow 'B' state then
                -1

            else
                0
    }
