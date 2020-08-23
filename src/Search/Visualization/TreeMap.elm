module Search.Visualization.TreeMap exposing (..)

import Axis exposing (tickSizeInner)
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Scale
import Search
import Search.Result exposing (Result(..))
import Shape
import Svg exposing (..)
import Svg.Attributes exposing (..)


vis : Search.Model a comparable -> Svg msg
vis model =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 1 1"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        (model.explored
            |> Dict.toList
            |> List.map
                (\( _, node ) ->
                    ( node
                    , Search.pathWithPosition model ( node.state, node )
                        |> List.reverse
                        |> List.map (\( _, _, ( a, b ) ) ->  ( toFloat a, toFloat b ))
                        |> List.foldl
                            (\( m, n ) ( x, width ) ->
                                 ( x + width * m /n, width / n )
                            )
                            ( 0, 1 )
                    )
                )
            |> List.map
                (\( node, ( x_, width_ ) ) ->
                    rect
                        [ x (String.fromFloat x_)
                        , y (String.fromFloat (node.pathCost * 0.1))
                        , width (String.fromFloat width_)
                        , height "0.1"
                        , fill "red"
                        , opacity "0.1"
                        , strokeWidth "0.001"
                        , stroke "black"
                        ]
                        []
                )
        )
