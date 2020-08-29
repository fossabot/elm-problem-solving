module Game exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe


type Player
    = PlayerA
    | PlayerB


other : Player -> Player
other player =
    if player == PlayerA then
        PlayerB

    else
        PlayerB


type alias Game a =
    { initialState : a
    , actions : a -> List a
    , player : a -> Player
    , terminalUtility : a -> Maybe (Player -> Float)
    , heuristic : a -> Player -> Float
    }



-- MINIMAX


maxValue : Game a -> a -> Maybe Int -> Maybe ( Player -> Float, a )
maxValue game state limit =
    let
        player =
            game.player state
    in
    case game.terminalUtility state of
        Just u ->
            Just ( u, state )

        Nothing ->
            if limit == Just 0 then
                Just ( game.heuristic state, state )

            else
                game.actions state
                    |> List.map (\a -> maxValue game a (Maybe.map ((+) -1) limit))
                    |> Maybe.values
                    |> List.foldl1
                        (\( v, b ) ( u, a ) ->
                            if v player > u player then
                                ( v, b )

                            else
                                ( u, a )
                        )


minimaxDecision game state limit =
    let
        player =
            game.player state
    in
    maxValue game state limit |> Maybe.map (\( u, a ) -> ( u player, a ))



-- ALPHA-BETA
