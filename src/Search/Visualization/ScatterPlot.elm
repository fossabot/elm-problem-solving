module Search.Visualization.ScatterPlot exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Search
import Svg exposing (..)
import Svg.Attributes exposing (..)


vis : Search.Model a comparable -> Svg msg
vis model =
    let
        dots : Dict ( Float, Float ) (List a)
        dots =
            model.explored
                |> Dict.toList
                |> List.map
                    (\( _, node ) ->
                        ( ( node.pathCost, model.problem.heuristic node.state ), [ node.state ] )
                    )
                |> Dict.fromListDedupe (++)

        maxDotSize =
            dots
                |> Dict.values
                |> List.map List.length
                |> List.foldl Basics.max 0
                |> toFloat
                |> sqrt

        maxHeuristic =
            model.problem.heuristic model.problem.initialState
    in
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 1.2 1.2"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        [ g [ transform "translate(0.1 0.1)" ]
            (dots
                |> Dict.toList
                |> List.map
                    (\( ( pathCost, heuristic ), states ) ->
                        circle
                            [ cx
                                (String.fromFloat
                                    (pathCost / model.maxPathCost)
                                )
                            , cy
                                (String.fromFloat
                                    (heuristic / maxHeuristic)
                                )
                            , r
                                (String.fromFloat
                                    (sqrt (toFloat (List.length states))
                                        / maxDotSize
                                        / Basics.max 10 (toFloat (Dict.size dots))
                                        / 2
                                    )
                                )
                            ]
                            []
                    )
            )
        ]
