module Search.Visualization.TreeMap exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import Maybe.Extra as Maybe
import Search
import Search.Result exposing (Result(..))


type alias Model comparable msg =
    { searchModel : Search.Model comparable comparable
    , msg : Maybe ( Float, comparable ) -> msg
    , visualizeState : comparable -> Html msg
    , tooltip : Maybe (Tooltip comparable)
    }


type alias Tooltip a =
    { node : Maybe ( Float, a )
    , position :
        { x : Float
        , y : Float
        }
    }


el : Model comparable msg -> Element msg
el model =
    column
        [ width fill
        , height fill
        , pointer
        , Events.onMouseLeave (model.msg Nothing)
        ]
        [ -- tooltip
          model.tooltip
            |> Maybe.map (tooltip model)
            |> Maybe.withDefault none
        , -- diagram
          box model True True ( 0, model.searchModel.problem.initialState )
        ]


html : Model comparable msg -> Html msg
html model =
    layout
        [ width fill, height fill ]
        (el model)


box :
    Model comparable msg
    -> Bool
    -> Bool
    -> ( Float, comparable )
    -> Element msg
box model withInfo flip ( pathCost, state ) =
    let
        children =
            Dict.get state model.searchModel.explored
                |> Maybe.map .children
                |> Maybe.join
                |> Maybe.map
                    (List.filter
                        (\( pathCost_, state_ ) ->
                            (model.searchModel.explored
                                |> Dict.get state_
                                |> Maybe.map .pathCost
                            )
                                == Just pathCost_
                        )
                    )
                |> Maybe.withDefault []

        c =
            1 - (pathCost / max model.searchModel.maxPathCost 5 * 0.8)

        isInSolutionPath =
            model.searchModel.solution
                |> Search.Result.map (Search.path model.searchModel >> List.map Tuple.second)
                |> Search.Result.withDefault []
                |> List.member state

        minimum_ =
            if isInSolutionPath then
                minimum 20

            else
                identity
    in
    columnOrRow flip
        ([ Background.color (rgb c c c)
         , Border.rounded (round (20 * c))
         , width (fillPortion 10)
         , height (fillPortion 10)
         ]
            ++ (if withInfo then
                    [ Background.color (rgb 1 1 1)
                    , Events.onMouseEnter (model.msg Nothing)
                    ]

                else
                    []
               )
        )
        [ if withInfo then
            info model ( pathCost, state )

          else
            Element.el
                [ width (fillPortion 10 |> minimum_)
                , height (fillPortion 10 |> minimum_)
                , Events.onMouseEnter (model.msg (Just ( pathCost, state )))
                ]
                none
        , columnOrRow
            (not flip)
            [ width (fillPortion 90)
            , height (fillPortion 90)
            , spacing (round (5 * c))
            ]
            (children
                |> List.map
                    (\( pathCost_, state_ ) ->
                        box
                            model
                            ((model.searchModel.solution
                                |> Search.Result.map (\( s, _ ) -> s == state_)
                                |> Search.Result.withDefault False
                             )
                                == True
                            )
                            (not flip)
                            ( pathCost_, state_ )
                    )
            )
        ]


columnOrRow : Bool -> (List (Attribute msg) -> List (Element msg) -> Element msg)
columnOrRow flip =
    if flip then
        column

    else
        row



-- INFO BOX ABOUT SEARCH STATE


info : Model state msg -> ( Float, state ) -> Element msg
info model ( pathCost, state ) =
    column
        [ spacing 20
        , centerX
        , padding 10
        ]
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
                (List.map htmlAttribute
                    [ style "left" (String.fromFloat (position.x + 10) ++ "px")
                    , style "top" (String.fromFloat (position.y + 10) ++ "px")
                    , style "z-index" "1"
                    ]
                    ++ [ width shrink
                       , height (maximum 0 shrink)
                       ]
                )
                (Element.el
                    [ Background.color (rgb 1 1 1)
                    , Border.rounded 5
                    , Border.glow (rgb 0.4 0.4 0.4) 2
                    , padding 10
                    ]
                    (info model n)
                )

        Nothing ->
            none
