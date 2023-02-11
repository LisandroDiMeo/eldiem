module Commons.ContentParser exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import String exposing (contains)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "http://localhost:8000/src/posts/article.txt"
      , expect = Http.expectString GotText
      }
  )

-- UPDATE

type Msg
  = GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
        let lines = String.split "\n" fullText
        in
        div [] (parseMd lines customStyles)

customStyles : Styles msg
customStyles = {
      h1 = []
    , h2 = []
    , h3 = []
    , h4 = []
    , p = []
    , ul = []
    , li = []
    , a = []
    , code = []
    , i = []
    , b = []
    , img = []
    , quote = []
    }

type Tag = Paragraph String | Image String

contentParser : List String -> List (Attribute msg) -> List (Attribute msg) -> List (Html msg)
contentParser content paragraphAttributes imageAttributes =
    let tagList = List.map toTag content
        deb0 = Debug.log "PreParse" content
    in
    List.map (\tag ->
        let deb = Debug.log "Parser" tag
        in
        case tag of
            Paragraph line -> toParagraphTag line paragraphAttributes
            Image src -> toImageTag src imageAttributes
    ) tagList


-- We assume that all images will come in strings of the following format:
-- "${image_src}"
toImageTag : String -> List (Attribute msg) -> Html msg
toImageTag line attributes =
    let imgSrc = String.dropLeft 2 line |> String.dropRight 1
    in
    img (src imgSrc :: attributes) []

toParagraphTag : String -> List (Attribute msg) -> Html msg
toParagraphTag line attributes =
    p attributes [text line]

toTag : String -> Tag
toTag line = if line |> contains "$" then Image line else Paragraph line

type MarkdownTag = H1 | H2 | H3 | H4 | P | IMG | LIST | CODE | QUOTE

matchers : List (String -> Maybe MarkdownTag)
matchers = [
      (\s -> if String.left 1 s == "#" && String.left 2 s /= "##" then Just H1 else Nothing)
    , (\s -> if String.left 2 s == "##" && String.left 3 s /= "###" then Just H2 else Nothing)
    , (\s -> if String.left 3 s == "###" && String.left 4 s /= "####" then Just H3 else Nothing)
    , (\s -> if String.left 4 s == "####" && String.left 5 s /= "#####" then Just H4 else Nothing)
    , (\s -> if String.left 3 s == "```" then Just CODE else Nothing)
    , (\s -> if String.startsWith "-" s then Just LIST else Nothing)
    , (\s -> if String.startsWith ">" s then Just QUOTE else Nothing)
    , (\s -> if String.startsWith "![" s then Just IMG else Nothing)
    , (\_ -> Just P)
    ]

type alias Styles msg = {
    h1 : List (Attribute msg) ,
    h2 : List (Attribute msg) ,
    h3 : List (Attribute msg) ,
    h4 : List (Attribute msg) ,
    p : List (Attribute msg) ,
    ul : List (Attribute msg) ,
    li : List (Attribute msg) ,
    a : List (Attribute msg) ,
    code : List (Attribute msg) ,
    i : List (Attribute msg) ,
    b : List (Attribute msg) ,
    img : List (Attribute msg) ,
    quote : List (Attribute msg)
    }

sublistAfterString : String -> List String -> List String
sublistAfterString str list =
  case list of
      [] -> []
      x::xs -> if String.contains str x then xs else sublistAfterString str xs

sublistBeforeString : String -> List String -> List String
sublistBeforeString str list =
  case list of
      [] -> []
      x::xs -> if String.contains str x then [] else x :: sublistBeforeString str xs

parseLists : List String -> List (Html msg)
parseLists lines =
    let isList = String.startsWith "-"
        isNestedList s = String.startsWith "  " s |> and <| String.startsWith "-" <| String.trimLeft s
    in
    case lines of
        [] -> []
        x::xs ->
            if isNestedList x then li [] [ ul [] [] ] :: parseLists xs
            else
                if isList x then li [] [text x] :: parseLists xs
                else []

type MarkdownList =
  Unordered
  | Ordered


parseMd : List (String) -> Styles msg -> List (Html msg)
parseMd lines styles =
    case lines of
        [] -> []
        line::nextLines ->
            case firstOf matchers line of
                H1 -> h1 styles.h1 [text <| String.dropLeft 1 line] :: parseMd nextLines styles
                H2 -> h2 styles.h2 [text <| String.dropLeft 2 line] :: parseMd nextLines styles
                H3 -> h3 styles.h3 [text <| String.dropLeft 3 line] :: parseMd nextLines styles
                H4 -> h4 styles.h4 [text <| String.dropLeft 4 line] :: parseMd nextLines styles
                CODE -> pre [] [ code [] [text <| String.join "\n" <| sublistBeforeString "```" <| nextLines] ] :: parseMd (sublistAfterString "```" nextLines) styles
                LIST -> pre [] [ code [] [text <| String.join "\n" <| sublistBeforeString "```" <| nextLines] ] :: parseMd (sublistAfterString "```" nextLines) styles
                P -> p styles.p [text line] :: parseMd nextLines styles
                _ -> []

firstOf : List (String -> Maybe MarkdownTag) -> String -> MarkdownTag
firstOf checkers elem =
    case checkers of
        [] -> P
        x::xs -> case x elem of
                    Just res -> res
                    Nothing -> firstOf xs elem
