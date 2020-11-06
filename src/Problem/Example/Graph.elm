module Problem.Example.Graph exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Encode exposing (encode, list, string)
import List.Extra as List
import Maybe.Extra as Maybe
import Problem exposing (Problem)


type alias Graph comparable =
    Dict comparable (List { stepCost : Float, result : comparable })


routeFinding : (comparable -> String) -> comparable -> comparable  -> Graph comparable -> Problem comparable
routeFinding nodeToString root goal graph =
    { initialState = root
    , actions = \a -> Dict.get a graph |> Maybe.withDefault []
    , heuristic = \_ -> 0
    , goalTest = (==) goal
    , stateToString = nodeToString
    }



{-
   touring : (comparable -> String) -> comparable -> Graph comparable -> Problem (List comparable)
   touring nodeToString root graph =
       { initialState = [ root ]
       , actions =
           \a ->
               List.head a
                   |> Maybe.map
                       (\h ->
                           Dict.get h graph
                               |> Maybe.withDefault []
                               |> List.map (\( pathCost, state ) -> ( pathCost, state :: a ))
                       )
                   |> Maybe.withDefault []
       , heuristic = \_ -> 0
       , goalTest = \a -> graph |> Dict.keys |> List.all (\city -> List.member city a)
       , stateToString = List.map nodeToString >> list string >> encode 0
       }


   {-| Lifts any state-space-problem to a touring problem in the state space. For example, lifts the route-finding problem between cities to a touring problem between cities. Not sure for which problems other than the route-finding problem it would make sense to apply this, but we could do it. Instead of lifting the route-finding problem it is recommended to use the `touring` problem creator directly, though, as it contains optimizations.
   -}
   toTouring : Problem a -> Problem (List a)
   toTouring problem =
       { initialState = [ problem.initialState ]
       , actions =
           \l ->
               List.head l
                   |> Maybe.map
                       (\h ->
                           problem.actions h
                               |> List.map (\( pathCost, state ) -> ( pathCost, state :: l ))
                       )
                   |> Maybe.withDefault []
       , heuristic = \_ -> 0
       , goalTest =
           \l ->
               l
                   |> List.all
                       (\a ->
                           problem.actions a |> List.all (\( _, child ) -> List.member child l)
                       )
       , stateToString = List.map problem.stateToString >> list string >> encode 0
       }


simpleTouring : Problem (List String)
simpleTouring =
    touring identity "Arad" romania.distance

-}

--


simpleRouteFinding : Problem String
simpleRouteFinding =
    routeFinding identity "Arad" "Bucharest" romania.distance





-- ROMANIA


{-| Converts the vertices-and-edges-list of a Janiczek-graph to a `Dict`-based graph.
-}
toDict =
    Dict.groupBy .from
        >> Dict.map (\k v -> v |> List.map (\{ data, to } -> { stepCost = data, result = to }))


romania =
    { distance = distance
    , bucharestDistance = bucharestDistance
    }


distance : Graph String
distance =
    [ { from = "Arad", to = "Zerind", data = 75 }
    , { from = "Arad", to = "Sibiu", data = 140 }
    , { from = "Arad", to = "Timisoara", data = 118 }
    , { from = "Zerind", to = "Oradea", data = 71 }
    , { from = "Oradea", to = "Sibiu", data = 151 }
    , { from = "Sibiu", to = "Fagaras", data = 99 }
    , { from = "Sibiu", to = "Rimnicu Vilcea", data = 80 }
    , { from = "Fagaras", to = "Bucharest", data = 211 }
    , { from = "Bucharest", to = "Urziceni", data = 85 }
    , { from = "Bucharest", to = "Giurgiu", data = 90 }
    , { from = "Bucharest", to = "Pitesti", data = 101 }
    , { from = "Urziceni", to = "Vaslui", data = 142 }
    , { from = "Urziceni", to = "Hirsova", data = 98 }
    , { from = "Vaslui", to = "Iasi", data = 92 }
    , { from = "Iasi", to = "Neamt", data = 87 }
    , { from = "Hirsova", to = "Eforie", data = 86 }
    , { from = "Pitesti", to = "Craiova", data = 138 }
    , { from = "Pitesti", to = "Rimnicu Vilcea", data = 97 }
    , { from = "Craiova", to = "Drobeta", data = 120 }
    , { from = "Craiova", to = "Rimnicu Vilcea", data = 146 }
    , { from = "Drobeta", to = "Mehadia", data = 75 }
    , { from = "Mehadia", to = "Lugoj", data = 70 }
    , { from = "Lugoj", to = "Timisoara", data = 111 }
    ]
        -- also add opposite direction for each edge
        |> List.concatMap (\({ from, to, data } as a) -> [ a, { from = to, to = from, data = data } ])
        |> toDict


straightLineDistance : Graph String
straightLineDistance =
    [ { from = "Bucharest", to = "Arad", data = 366 }
    , { from = "Bucharest", to = "Mehadia", data = 241 }
    , { from = "Bucharest", to = "Bucharest", data = 0 }
    , { from = "Bucharest", to = "Neamt", data = 234 }
    , { from = "Bucharest", to = "Craiova", data = 160 }
    , { from = "Bucharest", to = "Oradea", data = 380 }
    , { from = "Bucharest", to = "Drobeta", data = 242 }
    , { from = "Bucharest", to = "Pitesti", data = 100 }
    , { from = "Bucharest", to = "Eforie", data = 161 }
    , { from = "Bucharest", to = "Rimnicu Vilcea", data = 193 }
    , { from = "Bucharest", to = "Fagaras", data = 176 }
    , { from = "Bucharest", to = "Sibiu", data = 253 }
    , { from = "Bucharest", to = "Giurgiu", data = 77 }
    , { from = "Bucharest", to = "Timisoara", data = 329 }
    , { from = "Bucharest", to = "Hirsova", data = 151 }
    , { from = "Bucharest", to = "Urziceni", data = 80 }
    , { from = "Bucharest", to = "Iasi", data = 226 }
    , { from = "Bucharest", to = "Vaslui", data = 199 }
    , { from = "Bucharest", to = "Lugoj", data = 244 }
    , { from = "Bucharest", to = "Zerind", data = 374 }
    ]
        |> toDict


{-| Only the heuristic from/to Bucharest is known, so we instead of creating a more general heuristic, we create a specific heuristic which is only applicable fro routes from/to Bucharest.
-}
bucharestDistance : String -> Float
bucharestDistance a =
    straightLineDistance
        |> Dict.get "Bucharest"
        |> Maybe.map (List.find (\{ result } -> result == a))
        |> Maybe.join
        |> Maybe.map .stepCost
        |> Maybe.withDefault 1000
