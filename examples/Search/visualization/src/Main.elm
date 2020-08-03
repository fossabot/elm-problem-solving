module Main exposing (..)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import Dict exposing (Dict)
import Element
    exposing
        ( Attribute
        , Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , html
        , htmlAttribute
        , layout
        , mouseOver
        , none
        , padding
        , pointer
        , rgb
        , row
        , scale
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (div)
import Html.Attributes exposing (style)
import Json.Decode
import List.Extra as List
import Process
import Search
    exposing
        ( Model
        , Node
        , Parent(..)
        , Solution(..)
        , graphSearchStep
        , init
        , insertBy
        , insertFirst
        , insertLast
        , path
        )
import Search.Problems.NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle)
import Task


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body = [ layout [] (body model) ]
                }
        , init = init
        , update = update
        , subscriptions = \_ -> Browser.Events.onMouseMove (Json.Decode.map Move decode)
        }


body : Model -> Element Msg
body model =
    column [ width fill, height fill, pointer ]
        [ model.shown
            |> Maybe.map
                (\state ->
                    column
                        (List.map htmlAttribute
                            [ style "position" "absolute"
                            , style "left" (String.fromFloat (Tuple.first model.pos) ++ "px")
                            , style "top" (String.fromFloat (Tuple.second model.pos) ++ "px")
                            , style "z-index" "10"
                            ]
                            ++ [ Background.color (rgb 1 1 1)
                               , padding 20
                               , spacing 20
                               , Border.rounded 20
                               , Border.glow (rgb 0.5 0.5 0.5) 5
                               ]
                        )
                        [ visualizeNPuzzle state
                        , allNodes model.searchModel
                            |> List.find (\node -> node.state == state)
                            |> Maybe.map (path >> List.map (Debug.toString >> text) >> column [])
                            |> Maybe.withDefault none
                        ]
                )
            |> Maybe.withDefault none
        , visualizeCache model.visualizationCache
        ]


allNodes : Search.Model state -> List (Node state)
allNodes model =
    (case model.solution of
        Solution a ->
            [ a ]

        _ ->
            []
    )
        ++ model.explored
        ++ model.frontier


decode : Json.Decode.Decoder ( Float, Float )
decode =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


type alias State =
    List Int


type alias Cache state =
    { root : state
    , treeDict : Dict state (List state)
    }


type alias Model =
    { searchModel : Search.Model State
    , visualizationCache : Cache State
    , shown : Maybe State
    , pos : ( Float, Float )
    }


type Msg
    = NewModel (Search.Model State)
    | Show State
    | Hide State
    | Move ( Float, Float )


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


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.init (insertBy (\node -> node.pathCost + mediumEightPuzzle.heuristic node.state)) mediumEightPuzzle
        in
        ( { searchModel = initialModel
          , visualizationCache =
                { root = mediumEightPuzzle.initialState
                , treeDict = Dict.empty
                }
          , shown = Nothing
          , pos = ( 0, 0 )
          }
        , searchTask initialModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel m ->
            ( { model
                | searchModel = m
                , visualizationCache = updateCache m model.visualizationCache
              }
            , searchTask m
            )

        Show s ->
            ( { model | shown = Just s }, Cmd.none )

        Hide s ->
            ( if Just s == model.shown then
                { model | shown = Nothing }

              else
                model
            , Cmd.none
            )

        Move ( x, y ) ->
            ( { model | pos = ( x, y ) }, Cmd.none )


updateCache : Search.Model State -> Cache State -> Cache State
updateCache model cache =
    case model.explored of
        h :: _ ->
            { cache
                | treeDict =
                    cache.treeDict
                        |> Dict.insert h.state
                            (List.map .state (children h model.frontier))
                        |> (case model.solution of
                                Solution a ->
                                    List.head model.frontier
                                        |> Maybe.map (\node -> Dict.insert node.state [ a.state ])
                                        |> Maybe.withDefault identity

                                _ ->
                                    identity
                           )
            }

        _ ->
            cache


emptyNode : Node State
emptyNode =
    { state = [], parent = Nothing, pathCost = 0 }


root : List (Node State) -> Node State
root l =
    Maybe.withDefault emptyNode (List.find (\a -> a.parent == Nothing) l)


children : Node State -> List (Node State) -> List (Node State)
children parent l =
    List.filter (\a -> a.parent == Just (Parent parent)) l


type Hierarchy a
    = Hierarchy ( a, List (Hierarchy a) )


descendants : Node State -> List (Node State) -> Hierarchy State
descendants ancestor l =
    Hierarchy ( ancestor.state, children ancestor l |> List.map (\child -> descendants child l) )


properties : Int -> Int -> List (Attribute Msg)
properties level maxLevel =
    let
        s =
            1 - (toFloat level / max (toFloat maxLevel) 5 * 0.4)
    in
    [ width (fillPortion 80)
    , height (fillPortion 80)
    , padding 2
    , spacing 2
    , Border.rounded 20
    , Background.color (rgb s s s)
    ]


columnOrRow : Int -> Int -> (List (Element Msg) -> Element Msg)
columnOrRow level maxLevel =
    (if modBy 2 level == 0 then
        column

     else
        row
    )
    <|
        properties level maxLevel


rowOrColumn : Int -> Int -> (List (Element Msg) -> Element Msg)
rowOrColumn level maxLevel =
    (if modBy 2 level == 0 then
        row

     else
        column
    )
    <|
        properties level maxLevel


boxify : Int -> Int -> Hierarchy State -> Element Msg
boxify level maxDepth (Hierarchy ( a, h )) =
    columnOrRow level
        maxDepth
        (el [ centerX, centerY ] none
            :: (if List.length h > 0 then
                    [ rowOrColumn
                        level
                        maxDepth
                        (List.map (boxify (level + 1) maxDepth) h)
                    ]

                else
                    []
               )
        )


boxifyWithCache : Int -> Int -> Dict State (List State) -> State -> Element Msg
boxifyWithCache level maxDepth cache state =
    let
        children_ =
            Maybe.withDefault [] (Dict.get state cache)
    in
    columnOrRow level
        maxDepth
        (el
            [ width (fillPortion 20)
            , height (fillPortion 20)
            , Events.onMouseEnter (Show state)
            , Events.onMouseLeave (Hide state)
            ]
            (el [] none)
            :: (if List.length children_ > 0 then
                    [ rowOrColumn
                        level
                        maxDepth
                        (List.map
                            (boxifyWithCache (level + 1) maxDepth cache)
                            children_
                        )
                    ]

                else
                    []
               )
        )


depth : List (Node a) -> Int
depth l =
    l |> List.map (\a -> List.length (path a)) |> List.foldl max 0


visualizeNPuzzle : State -> Element Msg
visualizeNPuzzle state =
    column []
        (state
            |> List.groupsOf 3
            |> List.map
                (\row ->
                    row
                        |> List.map (String.fromInt >> text >> el [])
                )
            |> List.map (row [])
        )


visualization : Search.Model State -> Element Msg
visualization model =
    let
        l =
            allNodes model
    in
    boxify 0 (depth l) (descendants (root l) l)


visualizeCache : Cache State -> Element Msg
visualizeCache cache =
    boxifyWithCache 0 5 cache.treeDict cache.root
