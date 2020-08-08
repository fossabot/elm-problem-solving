module Search.Problem.Romania exposing (..)

import Graph exposing (Graph)


distance : Graph String Float
distance =
    Graph.fromVerticesAndEdges []
        ([ { from = "Arad", to = "Zerind", data = 75 }
         , { from = "Arad", to = "Sibiu", data = 140 }
         , { from = "Arad", to = "Timisoara", data = 118 }
         , { from = "Zerind", to = "Oradea", data = 71 }
         , { from = "Oradea", to = "Sibiu", data = 151 }
         , { from = "Sibiu", to = "Fagaras", data = 99 }
         , { from = "Sibiu", to = "RimnicuVilcea", data = 80 }
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
         , { from = "Pitesti", to = "RimnicuVilcea", data = 97 }
         , { from = "Craiova", to = "Drobeta", data = 120 }
         , { from = "Craiova", to = "RimnicuVilcea", data = 146 }
         , { from = "Drobeta", to = "Mehadia", data = 75 }
         , { from = "Mehadia", to = "Lugoj", data = 70 }
         , { from = "Lugoj", to = "Timisoara", data = 111 }
         ]
            |> List.map (\({ from, to, data } as a) -> [ a, { from = to, to = from, data = data } ])
            |> List.concat
        )


straightLineDistance : Graph String Float
straightLineDistance =
    Graph.fromVerticesAndEdges []
        [ { from = "Bucharest", to = "Arad", data = 366 }
        , { from = "Bucharest", to = "Mehadia", data = 241 }
        , { from = "Bucharest", to = "Bucharest", data = 0 }
        , { from = "Bucharest", to = "Neamt", data = 234 }
        , { from = "Bucharest", to = "Craiova", data = 160 }
        , { from = "Bucharest", to = "Oradea", data = 380 }
        , { from = "Bucharest", to = "Drobeta", data = 242 }
        , { from = "Bucharest", to = "Pitesti", data = 100 }
        , { from = "Bucharest", to = "Eforie", data = 161 }
        , { from = "Bucharest", to = "RimnicuVilcea", data = 193 }
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


{-| Only the heuristic from/to Bucharest is known, so we hardcode it.
-}
bucharestHeuristic : String -> Float
bucharestHeuristic a =
    Graph.getEdge a "Bucharest" straightLineDistance
        |> Maybe.withDefault 1000
