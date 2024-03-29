module Commons.ContentParser exposing (..)

import Browser
import Commons.TextDecoration exposing (buildHtmlText)
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
      { url = "http://localhost:8000/src/posts/post002.txt"
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
    , p = [
            class "post-paragraph"
        ]
    , ul = []
    , li = []
    , a = []
    , code = [
            class "language-kotlin"
        ]
    , pre = [
            class "language-kotlin"
        ]
    , i = []
    , b = []
    , img = (\styles url -> src url :: styles)
    , quote = []
    }

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
    pre : List (Attribute msg) ,
    i : List (Attribute msg) ,
    b : List (Attribute msg) ,
    img : List (Attribute msg) -> String -> List (Attribute msg) ,
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

takeLinesWhile : (String -> Bool) -> List String -> List String
takeLinesWhile condition list =
    case list of
        [] -> []
        x::xs -> if condition x then x :: takeLinesWhile condition xs else []

joinTuples : (appendable, appendable) -> (appendable, appendable) -> (appendable, appendable)
joinTuples (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

separateListsBy : (a -> Bool) -> List a -> (List a, List a)
separateListsBy condition list =
    case list of
        [] -> ([], [])
        x::xs ->
            if condition x then joinTuples ([x], []) (separateListsBy condition xs) else ([], x::xs)

type MarkdownList =
  Unordered
  | Ordered

markdownListToHtml : MarkdownList -> String -> Html msg
markdownListToHtml markdownList text =
  let
    listItems =
      String.lines text
        |> List.filter (\line -> String.trim line /= "")
        |> List.map (\item -> li [] [ Html.text <| String.dropLeft 2 item ])
  in
    case markdownList of
      Unordered ->
        ul [] listItems

      Ordered -> -- TODO: I need to fix this...
        ul [ ] (List.indexedMap (\index item -> li [] [ Html.text <| (String.fromInt (index + 1) ++ ". "), item ]) listItems)

getImageUrlFromImgMarkdownTag : String -> String
getImageUrlFromImgMarkdownTag tag =
    tag
    |> String.split "]"
    |> List.tail
    |> Maybe.withDefault []
    |> List.head
    |> Maybe.withDefault ""
    |> String.dropLeft 1
    |> String.dropRight 1

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
                CODE -> pre styles.pre [ code styles.code [text <| String.join "\n" <| sublistBeforeString "```" <| nextLines] ] :: parseMd (sublistAfterString "```" nextLines) styles
                LIST ->
                    let (listItems, nextItems) = separateListsBy (String.startsWith "-") (line::nextLines)
                        listString = String.join "\n" listItems
                    in
                    markdownListToHtml Unordered listString :: parseMd nextItems styles
                P -> p styles.p (buildHtmlText <| line) :: parseMd nextLines styles
                IMG -> img (styles.img [class "thumbnail-1"] <| getImageUrlFromImgMarkdownTag line) [] :: parseMd nextLines styles
                QUOTE -> blockquote styles.quote [text <| String.dropLeft 1 line] :: parseMd nextLines styles

firstOf : List (String -> Maybe MarkdownTag) -> String -> MarkdownTag
firstOf checkers elem =
    case checkers of
        [] -> P
        x::xs -> case x elem of
                    Just res -> res
                    Nothing -> firstOf xs elem
