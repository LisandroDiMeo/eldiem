module Components.Snackbar exposing (..)

-- Imports
import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (classList)
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

type alias Snackbar = {
        message: String
    }

init: () -> (Maybe Snackbar, Cmd Msg)
init _ = (Just (Snackbar "snackbarMessage"), Cmd.none )

-- UPDATE

type Msg
  = Show String | Hide

update : Msg -> Maybe Snackbar -> (Maybe Snackbar, Cmd Msg)
update msg snackbar =
    case msg of
        Show s -> (Just (Snackbar s), Cmd.none)
        Hide -> (Nothing, Cmd.none)

view : Maybe Snackbar -> Html Msg
view snackbarMonad =
    case snackbarMonad of
        Just snackbar ->
            div [classList [("snackbar-container", True)]] [
                p [] [text snackbar.message]
            ]
        Nothing -> h1 [] []


-- SUBSCRIPTIONS

subscriptions : Maybe Snackbar -> Sub Msg
subscriptions model =
  Sub.none