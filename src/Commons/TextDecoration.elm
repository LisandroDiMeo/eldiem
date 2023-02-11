module Commons.TextDecoration exposing (..)

import Html exposing (Html, b, em, text)
import Regex exposing (..)
import Browser

main =
  Browser.sandbox { init = init, update = update, view = view }

init : Model
init = Model ""

-- UPDATE

type Msg = OnImagePressed

update : Msg -> Model -> Model
update _ model = model

type alias Model = { text : String }

view : Model -> Html msg
view _ = toHtml "Hello my **good** *lord*"

toHtml : String -> Html msg
toHtml input =
  let
    italicRegex = Regex.fromString "\\*(.*?)\\*"
    boldRegex = Regex.fromString "\\*\\*(.*?)\\*\\*"
    italicBoldRegex = Regex.fromString "\\*\\*\\*(.*?)\\*\\*\\*"

    replaceMatch : Maybe Regex -> String -> String -> String -> String
    replaceMatch regex search replace text =
      case regex of
          Just rgx ->
              let
                match =
                  Regex.find rgx text
              in
                case match of
                  x::_ ->
                    String.replace search (replace ++ x.match ++ replace) text

                  [] ->
                    text
          Nothing -> text

    processText : String -> String
    processText text =
      let
        italicText = replaceMatch italicRegex "*" "<em>" text
        boldText = replaceMatch boldRegex "**" "<b>" italicText
      in
        boldText
  in
    Html.text (processText input)
