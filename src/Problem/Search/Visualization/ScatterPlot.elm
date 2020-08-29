module Problem.Search.Visualization.ScatterPlot exposing (..)

import Axis exposing (tickSizeInner)
import Color exposing (black)
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Scale
import Problem.Search as Search exposing (Result(..))
import TypedSvg exposing (..)
import TypedSvg.Attributes as Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)


scatterPlot : Search.Model a -> Svg msg
scatterPlot model =
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

        nX =
            dots
                |> Dict.keys
                |> List.map Tuple.first
                |> List.unique
                |> List.length

        nY =
            dots
                |> Dict.keys
                |> List.map Tuple.first
                |> List.unique
                |> List.length

        maxDotSize =
            dots
                |> Dict.values
                |> List.map List.length
                |> List.foldl Basics.max 0
                |> toFloat
                |> sqrt

        maxPathCost =
            model.maxPathCost

        maxHeuristic =
            model.problem.heuristic model.problem.initialState

        pathCostScale =
            Scale.linear ( 0, 500 ) ( 0, maxPathCost )

        heuristicScale =
            Scale.linear ( 0, 500 ) ( 0, maxHeuristic )
    in
    svg
        [ width (px 500)
        , height (px 500)
        , viewBox 0 0 1.3 1.3
        , Attributes.style "border: 1px dotted black"
        ]
        [ g [ transform [ Translate 0.2 0.1 ] ]
            ([ g
                [ transform [ Translate 0 1.1, Scale 0.002 0.002 ] ]
                [ Axis.bottom [ tickSizeInner 3 ] pathCostScale ]
             , g
                [ transform [ Translate -0.1 0, Scale 0.002 0.002 ] ]
                [ Axis.left [ tickSizeInner 3 ] heuristicScale ]
             ]
                ++ (dots
                        |> Dict.toList
                        |> List.map
                            (\( ( pathCost, heuristic ), states ) ->
                                g []
                                    [ circle
                                        [ cx (px (pathCost / maxPathCost))
                                        , cy (px (heuristic / maxHeuristic))
                                        , r
                                            (px
                                                (Basics.min 0.05
                                                    (sqrt (toFloat (List.length states))
                                                        / maxDotSize
                                                        / toFloat (Basics.max nX nY)
                                                        / 2
                                                    )
                                                )
                                            )
                                        ]
                                        []
                                    ]
                            )
                   )
                ++ (case model.solution of
                        Solution a ->
                            [ TypedSvg.path
                                [ fill PaintNone
                                , stroke (Paint black)
                                , opacity (Opacity 0.5)
                                , strokeWidth (px 0.002)
                                , d <|
                                    "M 0 1"
                                        ++ (a
                                                |> Search.path model
                                                |> List.map
                                                    (\( pathCost, state ) ->
                                                        "L" ++ String.fromFloat (pathCost / maxPathCost) ++ " " ++ String.fromFloat (model.problem.heuristic state / maxHeuristic)
                                                    )
                                                |> List.foldl (++) ""
                                           )
                                ]
                                []
                            ]

                        _ ->
                            []
                   )
            )
        ]
