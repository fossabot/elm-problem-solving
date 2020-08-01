module Games exposing (Game, maxValue, maximinDecision, minValue, minimaxDecision)

import List.Extra as List
import Maybe.Extra as Maybe


type alias Game a =
    { initialState : a
    , actions : a -> List a
    , terminalTest : a -> Bool
    , utility : a -> Float
    , heuristic : a -> Float
    }


max : Maybe comparable -> Maybe comparable -> Maybe comparable
max a b =
    case ( a, b ) of
        ( Nothing, _ ) ->
            b

        ( _, Nothing ) ->
            a

        ( Just aa, Just bb ) ->
            Just (Basics.max aa bb)


min : Maybe comparable -> Maybe comparable -> Maybe comparable
min a b =
    case ( a, b ) of
        ( Nothing, _ ) ->
            b

        ( _, Nothing ) ->
            a

        ( Just aa, Just bb ) ->
            Just (Basics.min aa bb)



-- MINIMAX


maxValue : Game state -> state -> Maybe Int -> Maybe Float
maxValue game state limit =
    if game.terminalTest state then
        Just (game.utility state)

    else if limit == Just 0 then
        Just (game.heuristic state)

    else
        game.actions state
            |> List.map (\a -> minValue game a (Maybe.map ((+) -1) limit))
            |> List.foldl1 max
            |> Maybe.join


minValue : Game state -> state -> Maybe Int -> Maybe Float
minValue game state limit =
    if game.terminalTest state then
        Just (game.utility state)

    else if limit == Just 0 then
        Just (game.heuristic state)

    else
        game.actions state
            |> List.map (\a -> maxValue game a (Maybe.map ((+) -1) limit))
            |> List.foldl1 min
            |> Maybe.join


minimaxDecision : Game state -> state -> Maybe Int -> Maybe ( Float, state )
minimaxDecision game state limit =
    game.actions state
        |> List.map
            (\action ->
                minValue game action (Maybe.map ((+) -1) limit)
                    |> Maybe.map (\utility -> ( utility, action ))
            )
        |> Maybe.values
        |> List.foldl1
            (\b a ->
                if Tuple.first a < Tuple.first b then
                    b

                else
                    a
            )


maximinDecision : Game state -> state -> Maybe Int -> Maybe ( Float, state )
maximinDecision game state limit =
    game.actions state
        |> List.map
            (\action ->
                maxValue game action (Maybe.map ((+) -1) limit)
                    |> Maybe.map (\utility -> ( utility, action ))
            )
        |> Maybe.values
        |> List.foldl1
            (\b a ->
                if Tuple.first a > Tuple.first b then
                    b

                else
                    a
            )



-- ALPHA-BETA
