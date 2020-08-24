module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Force exposing (..)
import Html exposing (Html, p, text)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Process
import Search
import Search.Problem exposing (Node)
import Search.Problem.Graph exposing (routeFinding)
import Search.Problem.NPuzzle as NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle, visualize)
import Search.Problem.Romania as Romania
import Search.Result exposing (Result(..))
import Search.Visualization.TreeMap as TreeMap
import Search.Visualization.TreeMap2 as TreeMap2
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Tuple


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State State)


type alias Node =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , id : State
    }


type alias Model =
    { searchModel : Search.Model State State
    , nodes : List (Force.Entity State {})
    , edges :
        List
            { source : State
            , target : State
            , distance : Float
            , strength : Maybe Float
            }
    }


main =
    Browser.document
        { view =
            \model ->
                { title = "Search of 8-Puzzle"
                , body = [ forceMap model ]
                }
        , init = init
        , update = update
        , subscriptions =
            \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialSearchModel =
                Search.bestFirst complexEightPuzzle
        in
        ( { searchModel = initialSearchModel
          , nodes =
                [ { id = initialSearchModel.problem.stateToComparable initialSearchModel.problem.initialState
                  , x = 0
                  , y = 0
                  , vx = 0
                  , vy = 0
                  }
                ]
          , edges = []
          }
        , searchTask initialSearchModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel m ->
            let
                nodes : List (Entity State {})
                nodes =
                    (m.newExplored
                        |> List.indexedMap
                            (\i { state, parent } ->
                                let
                                    parentNode =
                                        model.nodes |> List.find (\{ id } -> Just id == parent)
                                in
                                { id = m.problem.stateToComparable state
                                , x =
                                    (parentNode
                                        |> Maybe.map .x
                                        |> Maybe.withDefault 0
                                    )
                                        + 0.1
                                        * toFloat i
                                , y =
                                    (parentNode
                                        |> Maybe.map .y
                                        |> Maybe.withDefault 0
                                    )
                                        + 0.1
                                , vx = 0
                                , vy = 0
                                }
                            )
                    )
                        ++ model.nodes

                newNodes : List (Entity State {})
                newNodes =
                    computeSimulation
                        (simulation
                            [ center 0 0
                            , customLinks 1 model.edges
                            , manyBodyStrength -0.002 (List.map .id nodes)
                            ]
                        )
                        nodes
                        |> List.map (\({ x } as a) -> { a | x = x })

                newEdges =
                    let
                        _ =
                            Debug.log "n" (m.newExplored |> List.filter (\{ parent } -> parent == Nothing))
                    in
                    (m.newExplored
                        |> List.map
                            (\{ state, parent, pathCost } ->
                                Maybe.map
                                    (\parent_ ->
                                        { source = parent_
                                        , target = state
                                        , distance =
                                            pathCost
                                                - (m.explored
                                                    |> Dict.get (m.problem.stateToComparable parent_)
                                                    |> Maybe.map .pathCost
                                                    |> Maybe.withDefault 0
                                                  )
                                        , strength = Nothing
                                        }
                                    )
                                    parent
                            )
                        |> Maybe.values
                    )
                        ++ model.edges
            in
            ( { model
                | searchModel = m
                , nodes = newNodes
                , edges = newEdges
              }
              --, Cmd.none
            , searchTask m
            )


searchTask : Search.Model State State -> Cmd Msg
searchTask model =
    case model.solution of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 0
                    |> Task.andThen
                        (\_ ->
                            Task.succeed
                                (Search.nextN 10 model)
                        )
                )

        _ ->
            Cmd.none


forceMap : Model -> Html Msg
forceMap model =
    let
        minX =
            List.minimumBy .x model.nodes |> Maybe.map .x |> Maybe.withDefault 0

        minY =
            List.minimumBy .y model.nodes |> Maybe.map .y |> Maybe.withDefault 0

        maxX =
            List.maximumBy .x model.nodes |> Maybe.map .x |> Maybe.withDefault 0

        maxY =
            List.maximumBy .y model.nodes |> Maybe.map .y |> Maybe.withDefault 0
        cx = 1 / (maxX - minX)
        cy = 1 / (maxY - minY)
        offsetX = minX * cx
        offsetY = minY * cy
    in
    svg
        [ width "900"
        , height "900"
        , viewBox "0 0 1 1"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        ((model.nodes
            |> List.map
                (\entity ->
                    g
                        [ transform
                            ("translate ("
                                ++ String.fromFloat (entity.x * cx - offsetX)
                                ++ " "
                                ++ String.fromFloat (entity.y * cy - offsetY)
                                ++ ")"
                            )
                        ]
                        [ circle [ r "0.002" ] []

                        {--, Svg.text_
                            [ stroke "black", strokeWidth "0.1", transform "translate(0.005 0) scale(0.001) ", fontSize "10" ]
                            [ Svg.text (Debug.toString entity.id) ]
                            --}
                        ]
                )
         )
            ++ (model.edges
                    |> List.map
                        (\{ source, target } ->
                            Maybe.map2 (path cx cy offsetX offsetY)
                                (model.nodes |> List.find (\{ id } -> id == source))
                                (model.nodes |> List.find (\{ id } -> id == target))
                        )
                    |> Maybe.values
               )
        )


path cx cy offsetX offsetY e1 e2 =
    Svg.path
        [ d
            ("M "
                ++ String.fromFloat (e1.x * cx - offsetX)
                ++ " "
                ++ String.fromFloat (e1.y * cy - offsetY)
                ++ "L "
                ++ String.fromFloat (e2.x * cx - offsetX)
                ++ " "
                ++ String.fromFloat (e2.y * cy - offsetY)
            )
        , stroke "black"
        , strokeWidth "0.0002"
        ]
        []
