module Main exposing (..)

import Browser
import Html exposing (text)
import Search.Component exposing (SearchModel, breadthFirstSearch, searchInit)
import Search.NPuzzle exposing (simpleEightPuzzle)


type alias State =
    List Int


type Msg
    = SearchOn ( SearchModel State, SearchModel State -> Cmd Msg )


main : Program () (SearchModel State) Msg
main =
    let
        searchModel =
            searchInit simpleEightPuzzle
    in
    Browser.element
        { view = \model -> text (Debug.toString model)
        , init = \_ -> ( searchModel, breadthFirstSearch SearchOn searchModel )
        , update =
            \msg model ->
                case msg of
                    SearchOn ( result, callback ) ->
                        ( result, callback result )
        , subscriptions = always Sub.none
        }
