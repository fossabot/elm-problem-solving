module Main exposing (..)

import Browser
import Html exposing (p, text)
import Process
import Search exposing (Model, Solution(..), graphSearchStep, init, insertLast)
import Search.Problems.NPuzzle exposing (mediumEightPuzzle)
import Task


type alias State =
    List Int


type Msg
    = NewModel (Model State)


searchTask model =
    case model.solution of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 0
                    |> Task.andThen
                        (\_ -> Task.succeed (graphSearchStep model))
                )

        _ ->
            Cmd.none


init : () -> ( Model State, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.init insertLast mediumEightPuzzle
        in
        ( initialModel, searchTask initialModel )


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body =
                    [ p [] [ text (Debug.toString model.solution) ]
                    , p [] [ text (Debug.toString model) ]
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
