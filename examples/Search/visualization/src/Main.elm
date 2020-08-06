module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import List.Extra as List
import Process
import Search
    exposing
        ( Model
        , Parent(..)
        , Solution(..)
        , graphSearchStep
        , init
        , insertLast
        )
import Search.Problems.NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle)
import Search.Visualization as Visualization
import Task


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State)
    | Show (Maybe ( Float, State ))
    | Move ( Float, Float )


type alias Model =
    Visualization.Model State Msg


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body = [ body model ]
                }
        , init = init
        , update = update
        , subscriptions = \_ -> Browser.Events.onMouseMove (Json.Decode.map Move decode)
        }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.init insertLast mediumEightPuzzle
        in
        ( { searchModel = initialModel
          , msg = Show
          , visualizeState = visualizeNPuzzle
          , tooltip = Just { node = Nothing, position = ( 0, 0 ) }
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
            ( { model | tooltip = tooltip |> Maybe.map (\t -> { t | node = s }) }, Cmd.none )

        Move ( x, y ) ->
            ( { model | tooltip = tooltip |> Maybe.map (\t -> { t | position = ( x, y ) }) }, Cmd.none )


body : Model -> Html Msg
body model =
    Visualization.html model


decode : Json.Decode.Decoder ( Float, Float )
decode =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


searchTask : Search.Model State -> Cmd Msg
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


visualizeNPuzzle : State -> Html Msg
visualizeNPuzzle state =
    table []
        (state
            |> List.groupsOf 3
            |> List.map
                (List.map
                    (\a ->
                        a
                            |> String.fromInt
                            |> text
                            |> List.singleton
                            |> td
                                [ style "height" "1em"
                                , style "width" "1em"
                                , style "background-color"
                                    (if a == 0 then
                                        "rgb(200, 200, 200)"

                                     else
                                        "rgb(230, 230, 230)"
                                    )
                                , style "text-align" "center"
                                ]
                    )
                )
            |> List.map (tr [])
        )
