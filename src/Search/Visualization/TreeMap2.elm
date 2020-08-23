module Search.Visualization.TreeMap2 exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Lazy exposing (lazy, lazy2)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Maybe.Extra as Maybe
import Search
import Search.Result exposing (Result(..))


type alias Tooltip a =
    { node : Maybe ( Float, a )
    , position :
        { x : Float
        , y : Float
        }
    }


el :
    (Maybe ( Float, a ) -> msg)
    -> (a -> Html msg)
    -> Maybe (Tooltip a)
    -> Search.Model a comparable
    -> Element msg
el msg visualizeState tooltip_ model =
    column
        [ width fill
        , height fill
        , pointer
        ]
        [ -- tooltip
          tooltip_
            |> Maybe.map (lazy (tooltip model visualizeState))
            |> Maybe.withDefault none
        , -- diagram
          lazy2 (box msg visualizeState True True ( 0, model.problem.initialState )) (Maybe.map .node tooltip_ |> Maybe.join) model
        ]


html :
    (Maybe ( Float, a ) -> msg)
    -> (a -> Html msg)
    -> Maybe (Tooltip a)
    -> Search.Model a comparable
    -> Html msg
html msg visualizeState tooltip_ model =
    layout
        [ width fill, height fill ]
        (el msg visualizeState tooltip_ model)


box :
    (Maybe ( Float, a ) -> msg)
    -> (a -> Html msg)
    -> Bool
    -> Bool
    -> ( Float, a )
    -> Maybe ( Float, a )
    -> Search.Model a comparable
    -> Element msg
box msg visualizeState withInfo flip ( pathCost, state ) tooltipNode model =
    let
        children =
            Dict.get (model.problem.stateToComparable state) model.explored
                |> Maybe.map .children
                |> Maybe.join
                |> Maybe.map
                    (List.filter
                        (\( pathCost_, state_ ) ->
                            (model.explored
                                |> Dict.get (model.problem.stateToComparable state_)
                                |> Maybe.map .pathCost
                            )
                                == Just pathCost_
                        )
                    )
                |> Maybe.withDefault []

        c =
            1 - (pathCost / max model.maxPathCost 5 * 0.8)

        isInSolutionPath =
            model.solution
                |> Search.Result.map (Search.path model >> List.map Tuple.second)
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
         , width fill
         , height fill
         ]
            ++ (if withInfo then
                    [ Background.color (rgb 1 1 1) ]

                else
                    let
                        t =
                            if tooltipNode == Just ( pathCost, state ) then
                                1

                            else
                                0
                    in
                    [ Background.color (rgb (c + t * 0.5) (c - t * 0.2) (c - t * 0.2)) ]
               )
        )
        [ if withInfo then
            info model visualizeState ( pathCost, state )

          else
            Element.el
                [ width (fillPortion 10 |> minimum_)
                , height (fillPortion 10 |> minimum_)
                , Events.onMouseEnter (msg (Just ( pathCost, state )))
                , Events.onMouseLeave (msg Nothing)
                ]
                none
        , if List.length children > 0 then
            columnOrRow
                (not flip)
                [ width (fillPortion 90)
                , height (fillPortion 90)
                , spacing (round (5 * c))
                ]
                (children
                    |> List.map
                        (\( pathCost_, state_ ) ->
                            box
                                msg
                                visualizeState
                                ((model.solution
                                    |> Search.Result.map (\( s, _ ) -> s == state_)
                                    |> Search.Result.withDefault False
                                 )
                                    == True
                                )
                                (not flip)
                                ( pathCost_, state_ )
                                tooltipNode
                                model
                        )
                )

          else
            none
        ]


columnOrRow : Bool -> (List (Attribute msg) -> List (Element msg) -> Element msg)
columnOrRow flip =
    if flip then
        column

    else
        row



-- INFO BOX ABOUT THE SEARCH STATE


info :
    Search.Model a b
    -> (a -> Html msg)
    -> ( Float, a )
    -> Element msg
info searchModel visualizeState ( pathCost, state ) =
    column
        [ spacing 20
        , centerX
        , padding 10
        ]
        [ Element.el
            [ centerX ]
            (Element.html (visualizeState state))
        , column [ spacing 2, Font.size 12 ]
            [ infoRow "Path cost" pathCost
            , infoRow "Heuristic" (searchModel.problem.heuristic state)
            , infoRow "Sum" (pathCost + searchModel.problem.heuristic state)
            ]
        ]


infoRow : String -> Float -> Element msg
infoRow description number =
    row [ width fill, spacing 20 ]
        [ Element.el [ alignLeft ] (text description)
        , Element.el [ alignRight ] (text <| String.fromFloat number)
        ]



-- TOOLTIP


tooltip : Search.Model a b -> (a -> Html msg) -> Tooltip a -> Element msg
tooltip model visualizeState { node, position } =
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
                    (info model visualizeState n)
                )

        Nothing ->
            none
