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
import Svg.Keyed as Keyed


type alias VisFunc a comparable =
    Search.Model a comparable
    -> ( Float, a, ( Float, Float ) )
    -> Rect
    -> Rect


vis :
    VisFunc a comparable
    -> Search.Model a comparable
    -> Svg msg
vis visFunc model =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 1 1"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        [ Keyed.node "g"
            []
            (model.explored
                |> Dict.toList
                |> List.map
                    (\( _, node ) ->
                        ( node
                        , Search.pathWithPosition model ( node.state, node )
                            |> List.reverse
                            |> List.map (\( pathCost, state, ( m, n ) ) -> ( pathCost, state, ( toFloat m, toFloat n ) ))
                            |> List.foldl (visFunc model)
                                { x = 0
                                , y = 0
                                , width = 1
                                , height = 1
                                , toggle = True
                                , parentPathCost = 0
                                }
                        )
                    )
                |> List.map
                    (\( node, acc ) ->
                        ( Debug.toString node.state
                        , rect
                            [ x (String.fromFloat acc.x)
                            , y (String.fromFloat acc.y)
                            , width (String.fromFloat acc.width)
                            , height (String.fromFloat acc.height)
                            , fill "black"
                            , opacity "0.1"
                            , strokeWidth "0.002"

                            --, stroke "black"
                            --, rx "0.01"
                            ]
                            []
                        )
                    )
            )
        ]


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , toggle : Bool
    , parentPathCost : Float
    }


treeMap : VisFunc a comparable
treeMap _ =
    \( pathCost, _, ( m, n ) ) { x, y, width, height, toggle } ->
        let
            c =
                1.05
        in
        if toggle then
            let
                w =
                    width / (c * n + 1)
            in
            { x = x + c * (m + 1) * w
            , y = y
            , width = w
            , height = height
            , toggle = not toggle
            , parentPathCost = pathCost
            }

        else
            let
                h =
                    height / (c * n + 1)
            in
            { x = x
            , y = y + c * (m + 1) * h
            , width = width
            , height = h
            , toggle = not toggle
            , parentPathCost = pathCost
            }


tree : VisFunc a comparable
tree model =
    \( pathCost, _, ( m, n ) ) { x, y, width, height, toggle, parentPathCost } ->
        let
            c =
                1.05
        in
        let
            w =
                width / n
        in
        { x = x + m * w
        , y = parentPathCost / model.maxPathCost
        , width = w
        , height = (pathCost - parentPathCost) / model.maxPathCost
        , toggle = not toggle
        , parentPathCost = pathCost
        }
