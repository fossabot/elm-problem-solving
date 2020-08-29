module Problem exposing (Problem, Node, expand)

{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}



type alias Problem a =
    { initialState : a
    , actions : a -> List ( Float, a )
    , heuristic : a -> Float
    , goalTest : a -> Bool
    , stateToString : a -> String
    }


type alias Node a =
    { state : a
    , parent : Maybe a
    , pathCost : Float
    , children : Maybe (List ( Float, a ))
    }


expand : Problem a -> ( a, Node a ) -> ( Node a, List ( a, Node a ) )
expand problem ( state, node ) =
    let
        actions =
            problem.actions state

        children =
            List.map
                (\( stepCost, result ) ->
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
