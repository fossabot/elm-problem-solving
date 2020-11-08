module Main exposing (..)

import Browser
import Dict
import Html exposing (p, text)
import Problem.Example exposing (SlidingPuzzle, mediumEightPuzzle)
import Problem.Search as Search
import Problem.Search.Visual as Visual
import Process
import Task


type alias Model =
    { searchModel : Search.Model ProblemState
    , graphModel : Visual.GraphModel ProblemState
    , tooltipModel : Visual.TooltipModel Msg ProblemState
    }


type alias ProblemState =
    SlidingPuzzle


type Msg
    = NewModel (Search.Model ProblemState)
    | Show (Maybe (Search.Node ProblemState))
    | Move { x : Float, y : Float }


searchTask model =
    case model.result of
        Search.Pending ->
            Task.perform
                NewModel
                (Process.sleep 10
                    |> Task.andThen
                        (\_ -> Task.succeed (Search.next model))
                )

        _ ->
            Cmd.none


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            problem =
                mediumEightPuzzle

            initialSearchModel =
                Search.breadthFirst problem
        in
        ( { searchModel = initialSearchModel
          , graphModel = Visual.graph.init problem
          , tooltipModel = Visual.tooltip.init problem Show (Just Problem.Example.slidingPuzzleVisual)
          }
        , searchTask initialSearchModel
        )


main =
    Browser.document
        { view =
            \{ searchModel, tooltipModel, graphModel } ->
                { title = "Breadth-first search of 8-Puzzle"
                , body =
                    [ p [] [ text (searchModel.result |> Debug.toString) ]
                    , p [] [ text (searchModel.explored |> Dict.size |> String.fromInt) ]
                    , Visual.tooltip.view tooltipModel
                    , Visual.graph.view (Just tooltipModel) graphModel
                    ]
                }
        , init = init
        , update =
            \msg ({ tooltipModel } as model) ->
                case msg of
                    NewModel m ->
                        ( { model
                            | searchModel = m
                            , graphModel = Visual.graph.update model.graphModel m
                          }
                        , searchTask m
                        )

                    Show s ->
                        ( { model | tooltipModel = { tooltipModel | node = s } }, Cmd.none )

                    Move p ->
                        ( { model | tooltipModel = { tooltipModel | position = p } }, Cmd.none )
        , subscriptions = \_ -> Visual.tooltip.sub Move
        }
