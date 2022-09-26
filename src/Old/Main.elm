module Main exposing (main)
import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)

main : Program () Int Msg
main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement | Reset | MultiIncrement

update : Msg -> number -> number
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    Reset -> 
      model - model

    MultiIncrement ->
      model + 10

view : Int -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ], 
      div [] [ text (String.fromInt model) ], 
      button [ onClick Increment ] [ text "+" ],
      button [ onClick Reset ] [ text "R"],
      button [ onClick MultiIncrement] [ text "+10" ]
    ]