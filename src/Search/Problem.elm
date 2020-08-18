module Search.Problem exposing (..)

{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}

import Dict exposing (Dict)


type alias Problem a comparable =
    { initialState : a
    , actions : a -> List ( Float, a )
    , heuristic : a -> Float
    , goalTest : a -> Bool
    , stateToComparable : a -> comparable
    }


type alias Node a =
    { parent : Maybe a
    , pathCost : Float
    , children : Maybe (List a)
    }


emptyNode =
    { parent = Nothing
    , pathCost = 0
    , children = Nothing
    }


type Parent a
    = Parent (Node a)


expand : Problem a b -> ( a, Node a ) -> ( Node a, List ( a, Node a ) )
expand problem ( state, node ) =
    let
        children =
            List.map
                (\( stepCost, result ) ->
                    ( result
                    , { parent = Just state
                      , pathCost = node.pathCost + stepCost
                      , children = Nothing
                      }
                    )
                )
                (problem.actions state)
    in
    ( { node | children = Just (List.map Tuple.first children) }, children )
