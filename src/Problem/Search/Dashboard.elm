module Problem.Search.Dashboard exposing (document, Search(..), Visual(..))

{-|

@docs document, Search, Visual

-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Problem exposing (Problem)
import Problem.Example
import Problem.Search as Search exposing (Result(..))
import Problem.Search.Visual as Visual
import Problem.Search.Visual.Tooltip as Tooltip
import Process
import Svg exposing (Svg)
import Task


{-| Visual dashboard. We can use this as the `main` function in our application.
-}
document :
    { problem : Problem a
    , searches : List Search
    , visuals : List Visual
    , problemStateToHtml : Maybe (a -> Html (Msg a))
    }
    -> Program () (Model a) (Msg a)
document config =
    Browser.document
        { view =
            \model ->
                { title = "Search"
                , body = view model
                }
        , init = \_ -> init config
        , update = update
        , subscriptions = \_ -> Visual.tooltip.sub Move
        }


{-| Helper type for easily creating a visual dashboard.
-}
type Visual
    = Scatter
    | Tree
    | TreeMap
    | Graph


{-| Helper type for easily creating a visual dashboard.
-}
type Search
    = DepthFirst
    | BreadthFirst
    | UniformCost
    | Greedy
    | BestFirst
    | TreeDepthFirst
    | TreeBreadthFirst
    | TreeUniformCost
    | TreeGreedy
    | TreeBestFirst


fromSearch : Search -> ( String, Problem a -> Search.Model a )
fromSearch s =
    case s of
        DepthFirst ->
            ( "Depth-first graph search", Search.depthFirst )

        BreadthFirst ->
            ( "Breadth-first graph search", Search.breadthFirst )

        UniformCost ->
            ( "Uniform-cost graph search", Search.uniformCost )

        Greedy ->
            ( "Greedy graph search", Search.greedy )

        BestFirst ->
            ( "Best-first graph search", Search.bestFirst )

        TreeDepthFirst ->
            ( "Depth-first graph search", Search.treeDepthFirst )

        TreeBreadthFirst ->
            ( "Breadth-first graph search", Search.treeBreadthFirst )

        TreeUniformCost ->
            ( "Uniform-cost graph search", Search.treeUniformCost )

        TreeGreedy ->
            ( "Greedy graph search", Search.treeGreedy )

        TreeBestFirst ->
            ( "Best-first graph search", Search.treeBestFirst )


type Msg a
    = NewModels (List (Search.Model a))
    | Show (Maybe (Search.Node a))
    | Move { x : Float, y : Float }


type alias SearchAndGraphModel a =
    { name : String
    , search : Search.Model a
    , graph : Visual.GraphModel a
    }


type alias Model a =
    { searchesAndGraphs : List (SearchAndGraphModel a)
    , visuals : List Visual
    , tooltip : Visual.TooltipModel (Msg a) a
    }


init :
    { problem : Problem a
    , searches : List Search
    , visuals : List Visual
    , problemStateToHtml : Maybe (a -> Html (Msg a))
    }
    -> ( Model a, Cmd (Msg a) )
init {visuals, searches, problem, problemStateToHtml} =
    let
        searchAndGraphModels : List (SearchAndGraphModel a)
        searchAndGraphModels =
            searches
                |> List.map
                    (\s ->
                        let
                            ( name, search ) =
                                fromSearch s
                        in
                        { name = name
                        , search = search problem
                        , graph = Visual.graph.init problem
                        }
                    )
    in
    ( { searchesAndGraphs = searchAndGraphModels
      , visuals = visuals
      , tooltip = Visual.tooltip.init problem Show problemStateToHtml
      }
    , searchTask NewModels (searchAndGraphModels |> List.map .search)
    )


visual : Tooltip.Model msg a -> SearchAndGraphModel a -> Visual -> Svg msg
visual tooltip { search, graph } v =
    case v of
        Scatter ->
            Visual.scatter (Just tooltip) search

        Tree ->
            Visual.tree (Just tooltip) search

        TreeMap ->
            Visual.treeMap (Just tooltip) search

        Graph ->
            Visual.graph.view (Just tooltip) graph


view : Model a -> List (Html (Msg a))
view { visuals, searchesAndGraphs, tooltip } =
    [ Visual.tooltip.view tooltip
    , table []
        (tr []
            (List.map
                (\{ name } ->
                    td
                        [ style "font-weight" "bold"
                        , style "text-align" "center"
                        ]
                        [ text name ]
                )
                searchesAndGraphs
            )
            :: List.map
                (\v -> tr [] (List.map (\m -> td [] [ visual tooltip m v ]) searchesAndGraphs))
                visuals
        )
    ]


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg ({ tooltip, searchesAndGraphs } as model) =
    case msg of
        NewModels ms ->
            ( { model
                | searchesAndGraphs =
                    List.zip ms searchesAndGraphs
                        |> List.map
                            (\( m, { name, graph } ) ->
                                { name = name
                                , search = m
                                , graph = Visual.graph.update graph m
                                }
                            )
              }
            , searchTask NewModels ms
            )

        Show s ->
            ( { model | tooltip = { tooltip | node = s } }, Cmd.none )

        Move p ->
            ( { model | tooltip = { tooltip | position = p } }, Cmd.none )


searchTask : (List (Search.Model a) -> msg) -> List (Search.Model a) -> Cmd msg
searchTask msg searches =
    Task.perform
        msg
        (Process.sleep 100
            |> Task.andThen
                (\_ ->
                    Task.succeed
                        (searches
                            |> List.map
                                (\search ->
                                    case search.result of
                                        Pending ->
                                            Search.next search

                                        _ ->
                                            search
                                )
                        )
                )
        )
