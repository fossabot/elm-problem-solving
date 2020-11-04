module Problem.Search.Visualization.Tree exposing (tree, treeMap)

import Color exposing (black, red)
import Dict
import Dict.Extra as Dict
import Html.Events exposing (..)
import List.Extra as List
import Problem.Search as Search exposing (Model, Result(..))
import Problem.Search.Visualization.Tooltip as Tooltip
import Svg.Keyed as Keyed
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)


tree : Maybe (Tooltip.Model msg a) -> Model a -> Svg msg
tree =
    makeTreeLikeVis treeLayout


treeMap : Maybe (Tooltip.Model msg a) -> Model a -> Svg msg
treeMap =
    makeTreeLikeVis treeMapLayout


type alias Layout a =
    { sort : List ( String, Search.Node a ) -> List ( String, Search.Node a )
    , fold : Model a -> ( Float, ( Float, Float ) ) -> Rect -> Rect
    }


makeTreeLikeVis : Layout a -> Maybe (Tooltip.Model msg a) -> Model a -> Svg msg
makeTreeLikeVis layout tooltip ({ explored, problem } as model) =
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
                |> layout.sort
                |> List.map
                    (\( _, node ) ->
                        ( node
                        , Search.reversePathWithPosition model node
                            |> List.map (\( pathCost, _, ( m, n ) ) -> ( pathCost, ( toFloat m, toFloat n ) ))
                            |> List.foldl (layout.fold model)
                                { x = 0
                                , y = 0
                                , width = 1
                                , height = 1
                                , toggle = True
                                , parentPathCost = -1
                                }
                        )
                    )
                |> List.map
                    (\( node, acc ) ->
                        ( problem.stateToString node.state
                        , rect
                            ([ x (px acc.x)
                             , y (px acc.y)
                             , width (px acc.width)
                             , height (px acc.height)
                             , fill (Paint black)
                             , opacity (Opacity 0.1)
                             , stroke (Paint black)
                             , strokeWidth (px 0.002)
                             , strokeOpacity (Opacity 1)
                             ]
                                ++ tooltipProperties tooltip node
                            )
                            []
                        )
                    )
            )
        ]


tooltipProperties : Maybe (Tooltip.Model msg a) -> Search.Node a -> List (TypedSvg.Core.Attribute msg)
tooltipProperties tooltip node =
    case tooltip of
        Just t ->
            [ onMouseOver (t.msg (Just node))
            , onMouseOut (t.msg Nothing)
            ]
                ++ (case t.node of
                        Just n ->
                            if n.state == node.state then
                                [ fill (Paint red) ]

                            else
                                []

                        _ ->
                            []
                   )

        _ ->
            []


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , toggle : Bool
    , parentPathCost : Float
    }


treeMapLayout : Layout a
treeMapLayout =
    { -- we need to sort the list in order to display the inner nodes on top of the other ones
      sort = List.sortBy (\( _, node ) -> node.pathCost)
    , fold =
        \_ ( pathCost, ( m, n ) ) { x, y, width, height, toggle, parentPathCost } ->
            let
                topLevel =
                    parentPathCost == -1

                space =
                    if topLevel then
                        0

                    else
                        1

                c =
                    if topLevel then
                        1

                    else
                        1.05
            in
            if toggle then
                let
                    w =
                        width / (c * n + space)
                in
                { x = x + c * (m + space) * w
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
    }


treeLayout : Layout a
treeLayout =
    { -- no need to sort the list for the tree layout, since nodes are not displayed in a nested way
      sort = identity
    , fold =
        \model ( pathCost, ( m, n ) ) { x, width, toggle, parentPathCost } ->
            let
                w =
                    width / n
            in
            { x = x + m * w
            , y = pathCost / (model.maxPathCost + 1)
            , width = w
            , height = (pathCost - parentPathCost) / (model.maxPathCost + 1)
            , toggle = toggle
            , parentPathCost = pathCost
            }
    }
