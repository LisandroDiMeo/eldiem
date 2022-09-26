module Inputs exposing (..)

import Browser
import Html exposing (Html, Attribute, span, input, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html exposing (br)
import Html exposing (img)
import Navbar exposing (navbar)



-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { input : String, metersInput : String
  }


init : Model
init =
  { input = "", metersInput = "" }



-- UPDATE


type Msg
  = Change String | ChangeOtherInput String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newInput ->
      { model | input = newInput }
    ChangeOtherInput otherInput -> 
      { model | metersInput = otherInput } 



-- VIEW


mynav = navbar [ (Navbar.NavbarTab "" "image.png" (Just (Change ""))) ]

view : Model -> Html Msg
view model = 
    div []
    [
        mynav,
        case String.toFloat model.input of 
            Just celsius -> 
                viewConverter model.input "°C = " "°F" "blue" (String.fromFloat (celsius * 1.8 + 32)) "green" Change
            Nothing -> 
                viewConverter model.input "°C = " "°F" "blue" "???" "red" Change
        ,
        br [] []
        ,
        case String.toFloat model.metersInput of 
            Just meters -> 
                viewConverter model.metersInput "m = " "cm" "blue" (String.fromFloat (meters * 100)) "green" ChangeOtherInput     
            Nothing -> 
                viewConverter model.metersInput "m = " "cm" "blue" "???" "red" ChangeOtherInput
    ]
    


viewConverter : String -> String -> String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewConverter userInput fromValue toValue color equivalentValue borderColor convert =
  span []
    [ input [ value userInput, onInput convert, style "width" "40px", style "border-color" borderColor ] []
    , text fromValue
    , span [ style "color" color ] [ text equivalentValue ]
    , text toValue
    ]
    