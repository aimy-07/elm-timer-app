module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time



---- MODEL ----


type alias Model =
    { currentTime : Maybe Time.Posix
    , timeZone : Maybe Time.Zone
    , timerState : TimerState
    , timerCount : Int
    }


type TimerState
    = Setting
    | CountingDown
    | Pause
    | TimeUp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentTime = Nothing
      , timeZone = Nothing
      , timerState = Setting
      , timerCount = 0
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ClickedTimerCountIncrementBtn
    | ClickedTimerCountDecrementBtn
    | ClickedTimerStartBtn
    | ClickedTimerStopBtn
    | ClickedTimerResetBtn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                nextTimerCount =
                    if model.timerState == CountingDown then
                        model.timerCount - 1

                    else
                        model.timerCount

                nextTimerState =
                    if model.timerState == CountingDown && nextTimerCount == 0 then
                        TimeUp

                    else
                        model.timerState
            in
            ( { model
                | currentTime = Just time
                , timerCount = nextTimerCount
                , timerState = nextTimerState
              }
            , Cmd.none
            )

        AdjustTimeZone zone ->
            ( { model | timeZone = Just zone }, Cmd.none )

        ClickedTimerCountIncrementBtn ->
            let
                nextTimerCount =
                    if model.timerCount == settableMaxTimerCount then
                        0

                    else
                        model.timerCount + 30
            in
            ( { model | timerCount = nextTimerCount }, Cmd.none )

        ClickedTimerCountDecrementBtn ->
            let
                nextTimerCount =
                    if model.timerCount == 0 then
                        settableMaxTimerCount

                    else
                        model.timerCount - 30
            in
            ( { model | timerCount = nextTimerCount }, Cmd.none )

        ClickedTimerStartBtn ->
            ( { model | timerState = CountingDown }, Cmd.none )

        ClickedTimerStopBtn ->
            ( { model | timerState = Pause }, Cmd.none )

        ClickedTimerResetBtn ->
            ( { model | timerState = Setting, timerCount = 0 }, Cmd.none )


settableMaxTimerCount : Int
settableMaxTimerCount =
    60 * 60 - 30



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    case ( model.currentTime, model.timeZone ) of
        ( Just currentTime, Just timeZone ) ->
            div []
                [ viewClock currentTime timeZone
                , viewTimer model.timerState model.timerCount
                ]

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
                , style "transform" (calcStyleRotate <| (hour + minute / 60 + second / 3600) * 5)
                ]
                [ div [ class "clock_element clock_thin-hand" ] []
                , div [ class "clock_element clock_fat-hand" ] []
                ]
            , div
                [ class "clock_anchor clock_minute"
                , style "transform" (calcStyleRotate <| minute + second / 60)
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
                            toStringDoubleDigit n

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
    { top = String.fromFloat (-radius * Basics.cos theta) ++ "px"
    , left = String.fromFloat (radius * Basics.sin theta) ++ "px"
    }


calcStyleRotate : Float -> String
calcStyleRotate second =
    "rotate(" ++ String.fromFloat (second * 360 / 60) ++ "deg)"


toStringDoubleDigit : Int -> String
toStringDoubleDigit num =
    if num < 10 then
        "0" ++ String.fromInt num

    else
        String.fromInt num


viewTimer : TimerState -> Int -> Html Msg
viewTimer timerState timerCount =
    div [ class "timer" ]
        [ viewTimerText timerState timerCount
        , div [ class "timer_btn-container" ] []
        , img
            [ class "timer_plus-btn"
            , src "./img/icon_btn_up.png"
            , onClick ClickedTimerCountIncrementBtn
            ]
            []
        , img
            [ class "timer_minus-btn"
            , src "./img/icon_btn_down.png"
            , onClick ClickedTimerCountDecrementBtn
            ]
            []
        , viewStartStopBtn timerState
        , div
            [ class "timer_reset-btn"
            , onClick ClickedTimerResetBtn
            ]
            [ text "RESET" ]
        ]


viewTimerText : TimerState -> Int -> Html Msg
viewTimerText timerState timerCount =
    let
        sec =
            Basics.modBy 60 timerCount

        min =
            Basics.round <| Basics.toFloat (timerCount - sec) / 60

        timerCountText =
            toStringDoubleDigit min ++ " : " ++ toStringDoubleDigit sec

        isTimeUpClass =
            if timerState == TimeUp then
                "is-timeup"

            else
                ""
    in
    div
        [ class "timer_text"
        , class isTimeUpClass
        ]
        [ text timerCountText ]


viewStartStopBtn : TimerState -> Html Msg
viewStartStopBtn timerState =
    case timerState of
        Setting ->
            div
                [ class "timer_start-stop-btn"
                , onClick ClickedTimerStartBtn
                ]
                [ text "START" ]

        CountingDown ->
            div
                [ class "timer_start-stop-btn"
                , onClick ClickedTimerStopBtn
                ]
                [ text "STOP" ]

        Pause ->
            div
                [ class "timer_start-stop-btn"
                , onClick ClickedTimerStartBtn
                ]
                [ text "START" ]

        TimeUp ->
            div
                [ class "timer_start-stop-btn"
                , class "is-disabled"
                , disabled True
                ]
                [ text "START" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
