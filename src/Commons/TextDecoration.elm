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
view _ =
    let s = "Hello my *beautiful world*, today no *goodbyes* are welcome. Only **Bad feelings** will remain."
        a = toHtml s
        b = buildRemainderIntervals a (0,0,NoDecorator) True (String.length s) 0
        t = sortIntervals <| a ++ b
        orig = Debug.log "Original string is " s
        content = intervalsToHtml t s
        d0 = Debug.log "All intervals are" t
    in
    Html.div [] content

type TextDecorators = NoDecorator | Italic | Bold | BoldItalic

toHtml : String -> List (Int, Int, TextDecorators)
toHtml input =
  let
    italicRegex = Regex.fromString "\\*(.*?)\\*"
    boldRegex = Regex.fromString "\\*\\*(.*?)\\*\\*"
    italicBoldRegex = Regex.fromString "\\*\\*\\*(.*?)\\*\\*\\*"

    replaceMatch : Maybe Regex -> TextDecorators -> String -> String -> List (Int, Int, TextDecorators)
    replaceMatch regex decorator filter text =
      case regex of
          Just regexMatcher ->
              let
                match =
                  Regex.find regexMatcher text
              in
              case match of
                  [] -> []
                  x::xs ->
                    List.map (\m -> (m.index, m.index + String.length m.match, decorator)) <|
                    List.filter (\m ->
                            not <| String.contains filter m.match -- We remove those matches that are hard to exclude with regex.
                        ) (x::xs)
          Nothing -> []
  in
  replaceMatch italicRegex Italic "**" input
  ++ replaceMatch boldRegex Bold "***" input
  ++ replaceMatch italicBoldRegex BoldItalic "****" input

-- [(9,18) , (24, 32)] -> (0, 8), (18 + 1, 24 - 1)

buildRemainderIntervals : List (Int, Int, TextDecorators) -> (Int, Int, TextDecorators) -> Bool -> Int -> Int -> List (Int, Int, TextDecorators)
buildRemainderIntervals intervals (_,y,_) isFirst length accLength =
    case intervals of
        [] ->
            if accLength < length then [(y + 1, length, NoDecorator)] else []
        (a,b,c)::xs ->
            if isFirst && a > 0 then (0, a - 1, NoDecorator) :: buildRemainderIntervals xs (a,b,c) False length (accLength + b - a)
            else (y + 1, a - 1, NoDecorator) :: buildRemainderIntervals xs (a,b,c) False length (accLength + b - a)

sortIntervals : List (Int, Int, TextDecorators) -> List (Int, Int, TextDecorators)
sortIntervals intervals = List.sortBy (\(x,_,_) -> x) intervals

intervalsToHtml : List (Int, Int, TextDecorators) -> String -> List (Html msg)
intervalsToHtml intervals content =
    List.map (
        \(from, to, decorator) ->
            let slicer = String.slice from to
                sliced = slicer content
                d0 = Debug.log "Slice res is " sliced
            in
            case decorator of
                NoDecorator -> text <| (slicer content) ++ " "
                Italic -> Html.i [] [text <| addWhiteSpace <| String.dropLeft 1 <| String.dropRight 1 <| slicer content]
                Bold -> Html.b [] [text <| addWhiteSpace <| String.dropLeft 2 <| String.dropRight 2 <| slicer content]
                BoldItalic -> Html.em [] [ Html.b [] [text <| (addWhiteSpace) <| String.dropLeft 3 <| String.dropRight 3 <| slicer content]]
    ) intervals

addWhiteSpace : String -> String
addWhiteSpace s = s ++ " "