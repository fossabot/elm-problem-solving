module Problem.Search.Visual exposing
    ( scatter
    , tree, treeMap
    , GraphModel, graph
    , info, tooltip, Tooltip
    )

{-|


# Search progress

@docs scatter


# Search tree

These two visualizations present the search graph in the form of a tree or a tree map. This neglects that the search graph is actually not a tree but a graph. It allows for very neat and compact visualizations.

@docs tree, treeMap


# Search graph

This visualizes the graph as it is: As a graph.

@docs GraphModel, graph

I recommend to have a look at [this simple example](https://github.com/davidpomerenke/elm-problem-solving/blob/main/examples/3-graph-example) of how the graph data model can be embedded into an application.


# Tooltip

A tooltip is an info box displayed when hovering over an element with the mouse. It shows a visualization of the search state, and some basic information about its position in the search tree.

This is not yet supported for all problems and search diagrams.

@docs info, tooltip, Tooltip

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


{-| Scatterplot.
X-Axis: Search depth.
Y-Axis: Heuristic.
Size of dot: Number of explored search states.
When the search is finished, also displays the path from the root to the goal state.
-}
scatter : Search.Model a -> Svg msg
scatter =
    ScatterPlot.scatterPlot


{-| Tree of the searched states. Similar to `treeMap` but simpler layout.
-}
tree : Search.Model a -> Svg msg
tree =
    Tree.tree


{-| Treemap of the searched states. Similar to `tree` but more compact.
-}
treeMap : Search.Model a -> Svg msg
treeMap =
    Tree.treeMap


{-| A good graph visualization is computationally a bit more complicated, and therefore this visualization has its own data model that needs to be updated and queried. We need to embed it as a kind of sub-model into our application.
-}
type alias GraphModel =
    Graph.Model


{-| We use these three functions to initialize and update the graph data model, and finally, to display the actual graph.
-}
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
