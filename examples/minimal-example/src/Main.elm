module Main exposing (..)

import Browser
import Dict
import Html exposing (p, text)
import Problem.Example.SlidingPuzzle exposing (mediumEightPuzzle)
import Problem.Search
import Process
import Task


type alias State =
    List Int


type Msg
    = NewModel (Problem.Search.Model State)


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


init : () -> ( Problem.Search.Model State, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Problem.Search.bestFirst mediumEightPuzzle
        in
        ( initialModel, searchTask initialModel )


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body =
                    [ p [] [ text (model.result |> Debug.toString) ]
                    , p [] [ text (model.explored |> Dict.size |> String.fromInt) ]
                    ]
                }
        , init = init
        , update =
            \msg _ ->
                case msg of
                    NewModel model ->
                        ( model, searchTask model )
        , subscriptions = \_ -> Sub.none
        }
