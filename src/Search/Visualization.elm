module Search.Visualization exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import Search


type alias Model comparable msg =
    { searchModel : Search.Model comparable
    , msg : Maybe ( Float, comparable ) -> msg
    , visualizeState : comparable -> Html msg
    , tooltip : Maybe (Tooltip comparable)
    }


type alias Tooltip a =
    { node : Maybe ( Float, a )
    , position : ( Float, Float )
    }


el : Model comparable msg -> Element msg
el model =
    column
        [ width fill
        , height fill
        , pointer
        ]
        [ model.tooltip |> Maybe.map (tooltip model) |> Maybe.withDefault none
        , column [ width fill, height fill ]
            (let
                rootNode =
                    ( 0, model.searchModel.problem.initialState )
             in
             [ Element.el [ centerX, padding 20 ] (info model rootNode)
             , row
                [ width fill
                , height fill
                , Events.onMouseLeave (model.msg Nothing)
                ]
                (childRow model True [ rootNode ])
             ]
            )
        ]


html : Model comparable msg -> Html msg
html model =
    layout
        [ width fill, height fill ]
        (el model)


boxify :
    Model comparable msg
    -> Bool
    -> List ( Float, comparable )
    -> Element msg
boxify model flip path =
    case path of
        (( pathCost, state ) as h) :: _ ->
            columnOrRow
                flip
                (boxAttributes model.searchModel h)
                (let
                    empty =
                        Element.el
                            [ width (fillPortion 10)
                            , height (fillPortion 10)
                            , Events.onMouseEnter (model.msg (Just h))
                            ]
                            none
                 in
                 (case model.searchModel.solution of
                    Search.Solution a ->
                        if a.state == state then
                            Element.el
                                [ Events.onMouseEnter (model.msg Nothing) ]
                                (info model ( pathCost, state ))

                        else
                            empty

                    _ ->
                        empty
                 )
                    :: childRow model flip path
                )

        _ ->
            none


boxAttributes : Search.Model state -> ( Float, state ) -> List (Attribute msg)
boxAttributes model ( pathCost, state ) =
    let
        c =
            color pathCost model.maxPathCost

        b =
            c - 0.2
    in
    [ width fill
    , height fill
    , Border.rounded 20
    , Background.color (rgb c c c)
    , Border.color (rgb b b b)
    , Border.width 1
    ]
        ++ (case model.solution of
                Search.Solution a ->
                    if a.state == state then
                        [ Background.color (rgb 1 1 1), padding 20 ]

                    else
                        []

                _ ->
                    []
           )


childRow :
    Model comparable msg
    -> Bool
    -> List ( Float, comparable )
    -> List (Element msg)
childRow model flip path =
    let
        children =
            Maybe.withDefault [] (Dict.get path model.searchModel.explored)
    in
    if List.length children > 0 then
        [ columnOrRow
            (not flip)
            [ width (fillPortion 90)
            , height (fillPortion 90)
            , spacing 2
            , padding 2
            ]
            (children
                |> List.map
                    (\child ->
                        boxify
                            model
                            (not flip)
                            (child :: path)
                    )
            )
        ]

    else
        []


columnOrRow : Bool -> (List (Attribute msg) -> List (Element msg) -> Element msg)
columnOrRow flip =
    if flip then
        column

    else
        row


color : Float -> Float -> Float
color pathCost maxPathCost =
    1 - (pathCost / max maxPathCost 5 * 0.8)



-- STATE INFO BOX


info : Model state msg -> ( Float, state ) -> Element msg
info model ( pathCost, state ) =
    column
        [ spacing 20 ]
        [ Element.el
            [ centerX ]
            (Element.html (model.visualizeState state))
        , column [ spacing 2, Font.size 12 ]
            [ infoRow "Path cost" pathCost
            , infoRow "Heuristic" (model.searchModel.problem.heuristic state)
            , infoRow "Sum" (pathCost + model.searchModel.problem.heuristic state)
            ]
        ]


infoRow : String -> Float -> Element msg
infoRow description number =
    row [ width fill, spacing 20 ]
        [ Element.el [ alignLeft ] (text description)
        , Element.el [ alignRight ] (text <| String.fromFloat number)
        ]



-- TOOLTIP


tooltip : Model state msg -> Tooltip state -> Element msg
tooltip model { node, position } =
    case node of
        Just n ->
            Element.el
                (tooltipAttributes position)
                (Element.el
                    [ Background.color (rgb 1 1 1)
                    , Border.rounded 20
                    , Border.glow (rgb 0.5 0.5 0.5) 2
                    , padding 20
                    ]
                    (info model n)
                )

        Nothing ->
            none


tooltipAttributes : ( Float, Float ) -> List (Attribute msg)
tooltipAttributes ( x, y ) =
    List.map htmlAttribute
        [ style "left" (String.fromFloat (x + 10) ++ "px")
        , style "top" (String.fromFloat (y + 10) ++ "px")
        , style "z-index" "1"
        ]
        ++ [ width shrink
           , height (maximum 0 shrink)
           ]
