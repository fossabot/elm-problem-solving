module Search.Visualization.ScatterPlot exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Search
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)


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
        [ width (px 500)
        , height (px 500)
        , viewBox 0 0 1.2 1.2
        , TypedSvg.Attributes.style "border: 1px dotted black"
        ]
        [ g [ transform [ Translate 0.1 0.1] ]
            (dots
                |> Dict.toList
                |> List.map
                    (\( ( pathCost, heuristic ), states ) ->
                        circle
                            [ cx (px (pathCost / model.maxPathCost))
                            , cy (px (heuristic / maxHeuristic))
                            , r
                                (px
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
