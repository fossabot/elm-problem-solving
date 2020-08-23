module Search.Problem exposing (..)

{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}


type alias Problem a comparable =
    { initialState : a
    , actions : a -> List ( Float, a )
    , heuristic : a -> Float
    , goalTest : a -> Bool
    , stateToComparable : a -> comparable
    }


type alias Node a =
    { state : a
    , parent : Maybe a
    , pathCost : Float
    , children : Maybe (List ( Float, a ))
    }


expand : Problem a b -> ( a, Node a ) -> ( Node a, List ( a, Node a ) )
expand problem ( state, node ) =
    let
        actions =
            problem.actions state

        children =
            List.indexedMap
                (\i ( stepCost, result ) ->
                    ( result
                    , { state = result
                      , parent = Just state
                      , pathCost = node.pathCost + stepCost
                      , children = Nothing
                      }
                    )
                )
                actions

        updatedParent =
            { node
                | children =
                    children
                        |> List.map (\( state_, node_ ) -> ( node_.pathCost, state_ ))
                        |> Just
            }
    in
    ( updatedParent, children )
