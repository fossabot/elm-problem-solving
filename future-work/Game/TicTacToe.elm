module Game.TicTacToe exposing (threeInARow, ticTacToe, stateToString, stateFromString)

import Game exposing (Game, Player(..), other)
import List.Extra as List
import Maybe.Extra as Maybe


type Token
    = Token Player
    | Empty


type alias Board =
    List (List Token)


threeInARow : Player -> Board -> Bool
threeInARow player board =
    List.any
        (\row ->
            List.all ((==) (Token player)) row
        )
        board
        || List.any
            (\col ->
                List.all ((==) (Token player)) col
            )
            (List.transpose board)
        || (Maybe.map (List.getAt 0) (List.getAt 0 board)
                == Just (Just (Token player))
                && Maybe.map (List.getAt 1) (List.getAt 1 board)
                == Just (Just (Token player))
                && Maybe.map (List.getAt 2) (List.getAt 2 board)
                == Just (Just (Token player))
           )
        || (Maybe.map (List.getAt 2) (List.getAt 0 board)
                == Just (Just (Token player))
                && Maybe.map (List.getAt 1) (List.getAt 1 board)
                == Just (Just (Token player))
                && Maybe.map (List.getAt 0) (List.getAt 2 board)
                == Just (Just (Token player))
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


ticTacToe : Game Board
ticTacToe =
    { initialState = List.groupsOf 3 (List.repeat 9 Empty)
    , player =
        \state ->
            if List.count ((==) (Token PlayerA)) (List.concat state) > List.count ((==) (Token PlayerB)) (List.concat state) then
                PlayerB

            else
                PlayerA
    , actions =
        \board ->
            let
                player_ =
                    ticTacToe.player board
            in
            coordinates
                |> List.map
                    (\( y, x_ ) ->
                        let
                            p =
                                List.getAt y board
                                    |> Maybe.withDefault []
                                    |> List.getAt x_
                                    |> Maybe.withDefault Empty
                        in
                        if p == Empty then
                            board
                                |> List.setAt y
                                    (List.getAt y board
                                        |> Maybe.withDefault []
                                        |> List.setAt x_ (Token player_)
                                    )
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.values
    , terminalUtility =
        \board ->
            if threeInARow PlayerA board then
                Just
                    (\p ->
                        case p of
                            PlayerA ->
                                1

                            PlayerB ->
                                -1
                    )

            else if threeInARow PlayerB board then
                Just
                    (\p ->
                        case p of
                            PlayerA ->
                                -1

                            PlayerB ->
                                1
                    )

            else if List.count ((==) Empty) (List.concat board) == 0 then
                Just (\_ -> 0)

            else
                Nothing
    , heuristic = \_ _ -> 0
    }


stateToString : Board -> String
stateToString board =
    board
        |> List.map
            (\row ->
                row
                    |> List.map
                        (\tile ->
                            case tile of
                                Token PlayerA ->
                                    "x"

                                Token PlayerB ->
                                    "o"

                                Empty ->
                                    " "
                        )
                    |> String.concat
            )
        |> List.foldl1 (\a acc -> acc ++ "\n" ++ a)
        |> Maybe.withDefault ""


stateFromString : String -> Board
stateFromString board =
    board
        |> String.split "\n"
        |> List.map
            (\row ->
                row
                    |> String.split ""
                    |> List.map
                        (\a ->
                            case a of
                                "x" ->
                                    Token PlayerA

                                "o" ->
                                    Token PlayerB

                                _ ->
                                    Empty
                        )
            )
