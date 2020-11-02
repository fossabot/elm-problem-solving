module Main exposing (..)

import Browser
import Dict
import Html exposing (p, text)
import Problem.Example exposing (SlidingPuzzle, mediumEightPuzzle)
import Problem.Search
import Problem.Search.Visual exposing (GraphModel, graph)
import Process
import Task


type alias Model =
    { searchModel : Problem.Search.Model ProblemState
    , graphModel : GraphModel
    }


type alias ProblemState =
    SlidingPuzzle


type Msg
    = NewModel (Problem.Search.Model ProblemState)


searchTask model =
    case model.result of
        Problem.Search.Pending ->
            Task.perform
                NewModel
                (Process.sleep 10
                    |> Task.andThen
                        (\_ -> Task.succeed (Problem.Search.next model))
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
                Problem.Search.breadthFirst problem
        in
        ( { searchModel = initialSearchModel
          , graphModel = graph.init problem
          }
        , searchTask initialSearchModel
        )


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body =
                    [ p [] [ text (model.searchModel.result |> Debug.toString) ]
                    , p [] [ text (model.searchModel.explored |> Dict.size |> String.fromInt) ]
                    , graph.view model.graphModel
                    ]
                }
        , init = init
        , update =
            \msg model ->
                case msg of
                    NewModel m ->
                        ( { model
                            | searchModel = m
                            , graphModel = graph.update model.graphModel m
                          }
                        , searchTask m
                        )
        , subscriptions = \_ -> Sub.none
        }
