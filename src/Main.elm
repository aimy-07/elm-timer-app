module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time



---- MODEL ----


type alias Model =
    { currentTime : Maybe Time.Posix
    , timeZone : Maybe Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentTime = Nothing
      , timeZone = Nothing
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | currentTime = Just time }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | timeZone = Just zone }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    case ( model.currentTime, model.timeZone ) of
        ( Just currentTime, Just timeZone ) ->
            viewClock currentTime timeZone

        _ ->
            text ""


viewClock : Time.Posix -> Time.Zone -> Html msg
viewClock currentTime timeZone =
    let
        hour =
            Basics.toFloat (Time.toHour timeZone currentTime)

        minute =
            Basics.toFloat (Time.toMinute timeZone currentTime)

        second =
            Basics.toFloat (Time.toSecond timeZone currentTime)
    in
    div [ class "clock" ]
        [ div [ class "clock_centre" ]
            [ div [ class "clock_dynamic" ]
                (viewMinuteTexts ++ viewHourTexts)
            , div [ class "clock_expand clock_round clock_circle-1" ] []
            , div
                [ class "clock_anchor clock_hour"
                , style "transform" (calcStyleRotate <| hour * 5)
                ]
                [ div [ class "clock_element clock_thin-hand" ] []
                , div [ class "clock_element clock_fat-hand" ] []
                ]
            , div
                [ class "clock_anchor clock_minute"
                , style "transform" (calcStyleRotate minute)
                ]
                [ div [ class "clock_element clock_thin-hand" ] []
                , div [ class "clock_element clock_fat-hand clock_minute-hand" ] []
                ]
            , div
                [ class "clock_anchor clock_second"
                , style "transform" (calcStyleRotate second)
                ]
                [ div [ class "clock_element clock_second-hand" ] []
                ]
            , div [ class "clock_expand clock_round clock_circle-2" ] []
            , div [ class "clock_expand clock_round clock_circle-3" ] []
            ]
        ]


viewMinuteTexts : List (Html msg)
viewMinuteTexts =
    List.range 1 60
        |> List.map
            (\n ->
                if Basics.modBy 5 n == 0 then
                    let
                        minuteText =
                            if n < 10 then
                                "0" ++ String.fromInt n

                            else
                                String.fromInt n

                        stylePosition =
                            calcStylePosition (Basics.toFloat n / 60) 135
                    in
                    div
                        [ class "clock_minute-text"
                        , style "top" stylePosition.top
                        , style "left" stylePosition.left
                        ]
                        [ text minuteText ]

                else
                    let
                        styleRotate =
                            calcStyleRotate <| Basics.toFloat n
                    in
                    div
                        [ class "clock_anchor"
                        , style "transform" styleRotate
                        ]
                        [ div [ class "clock_element clock_minute-line" ] [] ]
            )


viewHourTexts : List (Html msg)
viewHourTexts =
    List.range 1 12
        |> List.map
            (\n ->
                let
                    hourText =
                        String.fromInt n

                    stylePosition =
                        calcStylePosition (Basics.toFloat n / 12) 105
                in
                div
                    [ class <| "clock_hour-text clock_hour-" ++ hourText
                    , style "top" stylePosition.top
                    , style "left" stylePosition.left
                    ]
                    [ text hourText ]
            )


calcStylePosition : Float -> Float -> { top : String, left : String }
calcStylePosition phase radius =
    let
        theta =
            phase * 2 * Basics.pi
    in
    { top = String.fromFloat (-radius * Basics.cos theta) ++ "px" -- TODO: toFixed(1)必要？
    , left = String.fromFloat (radius * Basics.sin theta) ++ "px" -- TODO: toFixed(1)必要？
    }


calcStyleRotate : Float -> String
calcStyleRotate second =
    "rotate(" ++ String.fromFloat (second * 360 / 60) ++ "deg)"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
