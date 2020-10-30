module Problem.Search.Visualization.Tree exposing (tree, treeMap)

import Color exposing (black)
import Dict
import Dict.Extra as Dict
import List.Extra as List
import Problem.Search as Search exposing (Model(..), Node(..), Result(..))
import Svg.Keyed as Keyed
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)


tree : Model a -> Svg msg
tree =
    makeTreeLikeVis treeLayout


treeMap : Model a -> Svg msg
treeMap =
    makeTreeLikeVis treeMapLayout


type alias Layout a =
    Model a
    -> ( Float, a, ( Float, Float ) )
    -> Rect
    -> Rect


makeTreeLikeVis :
    Layout a
    -> Model a
    -> Svg msg
makeTreeLikeVis layout ((Model { explored, problem }) as model) =
    svg
        [ width (px 500)
        , height (px 500)
        , viewBox 0 0 1 1
        , TypedSvg.Attributes.style "border: 1px dotted black"
        ]
        [ Keyed.node "g"
            []
            (explored
                |> Dict.toList
                |> List.map
                    (\( _, node ) ->
                        ( node
                        , Search.pathWithPosition model node
                            |> List.reverse
                            |> List.map (\( pathCost, state, ( m, n ) ) -> ( pathCost, state, ( toFloat m, toFloat n ) ))
                            |> List.foldl (layout model)
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
                    (\( Node node, acc ) ->
                        ( problem.stateToString node.state
                        , rect
                            [ x (px acc.x)
                            , y (px acc.y)
                            , width (px acc.width)
                            , height (px acc.height)
                            , fill (Paint black)
                            , opacity (Opacity 0.1)
                            , strokeWidth (px 0.002)
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


treeMapLayout : Layout a
treeMapLayout _ ( pathCost, _, ( m, n ) ) { x, y, width, height, toggle } =
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


treeLayout : Layout a
treeLayout (Model model) ( pathCost, _, ( m, n ) ) { x, width, toggle, parentPathCost } =
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
