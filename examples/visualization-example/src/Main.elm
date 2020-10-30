module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (p, text)
import Json.Decode
import Problem.Example exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle, slidingPuzzleVisual, routeFinding)
import Problem.Search as Search exposing (Result(..))
import Problem.Search.Visual as Visual
import Process
import Task


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State)
    | Show (Maybe ( Float, State ))
    | Move { x : Float, y : Float }


type alias Model =
    { searchModel : Search.Model State 
    , tooltip : Visual.Tooltip State 
    }


main =
    Browser.document
        { view =
            \{ tooltip, searchModel } ->
                { title = "Search of 8-Puzzle"
                , body = [ Visual.scatter searchModel ]
                }
        , init = init
        , update = update
        , subscriptions =
            \_ -> Sub.none

        --Browser.Events.onMouseMove (Json.Decode.map Move decodeMove)
        }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.greedy complexEightPuzzle
        in
        ( { searchModel = initialModel
          , tooltip = { node = Nothing, position = { x = 0, y = 0 } }
          }
        , searchTask initialModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ tooltip } as model) =
    case msg of
        NewModel m ->
            ( { model | searchModel = m }
            , searchTask m
            )

        Show s ->
            ( { model | tooltip = { tooltip | node = s } }, Cmd.none )

        Move p ->
            ( { model | tooltip = { tooltip | position = p } }, Cmd.none )


decodeMove : Json.Decode.Decoder { x : Float, y : Float }
decodeMove =
    Json.Decode.map2 (\a b -> { x = a, y = b })
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


searchTask : Search.Model State -> Cmd Msg
searchTask model =
    case model.result of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 300
                    |> Task.andThen
                        (\_ -> Task.succeed (Search.next model))
                )

        _ ->
            Cmd.none
