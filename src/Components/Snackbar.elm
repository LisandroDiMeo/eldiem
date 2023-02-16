module Components.Snackbar exposing (..)

-- Imports
import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (classList, style)
import Html.Events exposing (onClick)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias SnackbarContent = {
        message: String
    }

type Snackbar = AppearingSnackbar SnackbarContent | DisappearingSnackbar SnackbarContent

init: () -> (Snackbar, Cmd Msg)
init _ = (AppearingSnackbar (SnackbarContent "snackbarMessage"), Cmd.none )

-- UPDATE

type Msg
  = Show String | Hide String

update : Msg -> Snackbar -> (Snackbar, Cmd Msg)
update msg snackbar =
    case msg of
        Show s -> (AppearingSnackbar (SnackbarContent s), Cmd.none)
        Hide s -> (DisappearingSnackbar (SnackbarContent s), Cmd.none)

view : Snackbar -> Html Msg
view snackbar =
    case snackbar of
        AppearingSnackbar snackbarContent ->
            div [classList [("snackbar-container", True), ("appearing", True), ("absolute-bottom", True)]] [
                p [style "margin-left" "32px"] [text snackbarContent.message] ,
                p [style "margin-right" "32px" , onClick <| Hide snackbarContent.message] [text "X"]
            ]
        DisappearingSnackbar snackbarContent ->
            div [classList [("snackbar-container", True), ("disappearing", True), ("absolute-bottom", True)]] [
                p [style "margin-left" "32px"] [text snackbarContent.message]
            ]


-- SUBSCRIPTIONS

subscriptions : Snackbar -> Sub Msg
subscriptions model =
  Sub.none