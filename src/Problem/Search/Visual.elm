module Problem.Search.Visual exposing
    ( scatter
    , tree, treeMap
    , GraphModel, graph
    , TooltipModel, tooltip
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

@docs TooltipModel, tooltip

-}

import Html exposing (Html)
import Problem exposing (Problem)
import Problem.Search as Search
import Problem.Search.Visual.Graph as Graph
import Problem.Search.Visual.ScatterPlot as ScatterPlot
import Problem.Search.Visual.Tooltip as Tooltip
import Problem.Search.Visual.Tree as Tree
import Svg exposing (Svg)


{-| Scatterplot.
X-Axis: Search depth.
Y-Axis: Heuristic.
Size of dot: Number of explored search states.
When the search is finished, also displays the path from the root to the goal state.
-}
scatter : Maybe (TooltipModel msg a) -> Search.Model a -> Svg msg
scatter =
    ScatterPlot.scatterPlot


{-| Tree of the searched states. Similar to `treeMap` but simpler layout.
-}
tree : Maybe (TooltipModel msg a) -> Search.Model a -> Svg msg
tree =
    Tree.tree


{-| Treemap of the searched states. Similar to `tree` but more compact.
-}
treeMap : Maybe (TooltipModel msg a) -> Search.Model a -> Svg msg
treeMap =
    Tree.treeMap



-- GRAPH


{-| A good graph visualization is computationally a bit more complicated, and therefore this visualization has its own data model that needs to be updated and queried. We need to embed it as a kind of sub-model into our application.
-}
type alias GraphModel a =
    Graph.Model a


{-| We use these three functions to initialize and update the graph data model, and finally, to display the actual graph.
-}
graph :
    { init : Problem a -> GraphModel a
    , update : GraphModel a -> Search.Model a -> GraphModel a
    , view : Maybe (TooltipModel msg a) -> GraphModel a -> Html msg
    }
graph =
    { init = Graph.init
    , update = Graph.update
    , view = Graph.view
    }



-- TOOLTIP


{-| -}
type alias TooltipModel msg a =
    Tooltip.Model msg a


{-| -}
tooltip :
    { init :
        Problem a
        -> (Maybe (Search.Node a) -> msg)
        -> Maybe (a -> Html msg)
        -> TooltipModel msg a
    , view : TooltipModel msg a -> Html msg
    , sub : ({ x : Float, y : Float } -> msg) -> Sub msg
    }
tooltip =
    { init = Tooltip.init
    , view = Tooltip.view
    , sub = Tooltip.sub
    }
