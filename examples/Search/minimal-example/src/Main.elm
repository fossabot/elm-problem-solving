module Main exposing (..)

import Browser
import Dict
import Html exposing (p, text)
import Process
import Search
import Search.Result
import Search.Problem.NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle)
import Task


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State State)


searchTask model =
    case model.solution of
        Search.Pending ->
            Task.perform
                NewModel
                (Process.sleep 0
                    |> Task.andThen
                        (\_ -> Task.succeed (Search.next model))
                )

        _ ->
            Cmd.none


init : () -> ( Search.Model State State, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.bestFirst mediumEightPuzzle
        in
        ( initialModel, searchTask initialModel )


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body =
                    [ p [] [ text (model.solution |> Search.Result.map .state |> Debug.toString) ]
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
