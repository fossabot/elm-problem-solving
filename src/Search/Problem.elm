module Search.Problem exposing (..)

{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}


type alias Problem a =
    { initialState : a
    , actions : a -> List ( Float, a )
    , heuristic : a -> Float
    , goalTest : a -> Bool
    }


type alias Node a =
    { state : a
    , parent : Maybe (Parent a)
    , pathCost : Float
    }


type Parent a
    = Parent (Node a)


path : Node a -> List ( Float, a )
path node =
    case node.parent of
        Just (Parent p) ->
            ( node.pathCost, node.state ) :: path p

        Nothing ->
            [ ( node.pathCost, node.state ) ]


expand : Problem a -> Node a -> List (Node a)
expand problem node =
    List.map
        (\( stepCost, result ) ->
            { state = result
            , parent = Just (Parent node)
            , pathCost = node.pathCost + stepCost
            }
        )
        (problem.actions node.state)
