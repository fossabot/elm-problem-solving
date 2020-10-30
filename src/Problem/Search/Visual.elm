module Problem.Search.Visual exposing (info, scatter, tree, treeMap, graph, tooltip, Tooltip)

{-|

@docs info, scatter, tree, treeMap, graph, tooltip, Tooltip

-}

import Element exposing (Element)
import Html exposing (Html)
import Problem exposing (Problem)
import Problem.Search as Search
import Problem.Search.Visualization.Graph as Graph
import Problem.Search.Visualization.Info as Info
import Problem.Search.Visualization.ScatterPlot as ScatterPlot
import Problem.Search.Visualization.Tree as Tree
import Svg exposing (Svg)


{-| -}
info :
    Problem a
    -> (a -> Html msg)
    -> ( Float, a )
    -> Element msg
info =
    Info.info


{-| -}
type alias Tooltip a =
    Info.Tooltip a


{-| -}
tooltip : Problem a -> (a -> Html msg) -> Tooltip a -> Element msg
tooltip =
    Info.tooltip


{-| -}
scatter : Search.Model a -> Svg msg
scatter =
    ScatterPlot.scatterPlot


{-| -}
tree : Search.Model a -> Svg msg
tree =
    Tree.tree


{-| -}
treeMap : Search.Model a -> Svg msg
treeMap =
    Tree.treeMap


{-| -}
graph :
    { init : Problem a -> Graph.Model
    , update : Graph.Model -> Search.Model a -> Graph.Model
    , view : Graph.Model -> Html msg
    }
graph =
    { init = Graph.init
    , update = Graph.update
    , view = Graph.view
    }
