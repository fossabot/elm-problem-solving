module Search.Problem.Graph exposing (..)

import Graph exposing (..)
import Search exposing (Problem)


routeFinding : a -> a -> Graph a Float -> Problem a
routeFinding root goal graph =
    { initialState = root
    , actions =
        \a ->
            Graph.outgoingEdgesWithData a graph
                |> List.map (\( vertex, data ) -> ( data, vertex ))
    , heuristic = \_ -> 0
    , goalTest = (==) goal
    }
