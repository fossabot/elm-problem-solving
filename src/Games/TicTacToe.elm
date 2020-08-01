module Games.TicTacToe exposing (player, threeInARow, ticTacToe)

import Games exposing (Game)
import List.Extra as List
import Maybe.Extra as Maybe


type alias State =
    List (List Char)


player : State -> Char
player state =
    if List.count ((==) 'x') (List.concat state) > List.count ((==) 'o') (List.concat state) then
        'o'

    else
        'x'


threeInARow : Char -> State -> Bool
threeInARow player_ state =
    List.any
        (\row ->
            List.all ((==) player_) row
        )
        state
        || List.any
            (\col ->
                List.all ((==) player_) col
            )
            (List.transpose state)
        || (Maybe.map (List.getAt 0) (List.getAt 0 state)
                == Just (Just player_)
                && Maybe.map (List.getAt 1) (List.getAt 1 state)
                == Just (Just player_)
                && Maybe.map (List.getAt 2) (List.getAt 2 state)
                == Just (Just player_)
           )
        || (Maybe.map (List.getAt 2) (List.getAt 0 state)
                == Just (Just player_)
                && Maybe.map (List.getAt 1) (List.getAt 1 state)
                == Just (Just player_)
                && Maybe.map (List.getAt 0) (List.getAt 2 state)
                == Just (Just player_)
           )


coordinates : List ( Int, Int )
coordinates =
    [ ( 0, 0 )
    , ( 0, 1 )
    , ( 0, 2 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 1, 2 )
    , ( 2, 0 )
    , ( 2, 1 )
    , ( 2, 2 )
    ]


ticTacToe : Game State
ticTacToe =
    { initialState = List.groupsOf 3 (List.repeat 9 ' ')
    , actions =
        \state ->
            coordinates
                |> List.map
                    (\( y, x ) ->
                        let
                            p =
                                List.getAt y state
                                    |> Maybe.withDefault []
                                    |> List.getAt x
                                    |> Maybe.withDefault ' '
                        in
                        if p == ' ' then
                            state
                                |> List.setAt y
                                    (List.getAt y state
                                        |> Maybe.withDefault []
                                        |> List.setAt x (player state)
                                    )
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.values
    , terminalTest =
        \state ->
            List.count ((==) ' ') (List.concat state)
                == 0
                || threeInARow 'x' state
                || threeInARow 'o' state
    , utility =
        \state ->
            if threeInARow 'x' state then
                1

            else if threeInARow 'o' state then
                -1

            else
                0
    , heuristic = \_ -> 0
    }
