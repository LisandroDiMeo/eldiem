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
      div [] (parseMd fullText customStyles)

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

--- It assumes that the firsts backticks were removed.
parseCodeTag : List String -> String
parseCodeTag lines =
    let x = List.foldr (\line rec -> if String.startsWith "```" line then rec else line ++ rec) "" lines
        d0 = Debug.log "AAA" x
        d1 = Debug.log "BBB" lines
    in
    List.foldr (\line rec -> if String.startsWith "```" line then rec else line ++ rec) "" lines

-- TODO: Currently this doesnt work for nested lists
parseListTag : List String -> List (Html msg)
parseListTag lines = List.foldr (
    \line rec -> if not <| String.startsWith "-" line then rec else (li [] [text <| String.left 1 line])::rec
    ) [] lines

remainderFoldr : (a -> List a -> b -> b) -> b -> List a -> b
remainderFoldr f acc list =
                   case list of
                       [] -> acc
                       x::xs -> f x xs (remainderFoldr f acc xs)

remainderFoldr2 : (a -> List a -> b -> b) -> (a -> Bool) -> (List a -> List a) -> b -> List a -> b
remainderFoldr2 f shouldSkip g acc list =
                   case list of
                       [] -> acc
                       x::xs ->
                            let ys = if shouldSkip x then xs else g xs
                            in
                            f x xs (remainderFoldr2 f shouldSkip g acc ys)

sublistAfterBacktics : List String -> List String
sublistAfterBacktics list =
  List.foldr (\str acc -> if String.contains "```" str then [] else str :: acc) [] <| List.reverse list

sublistBeforeBacktics : List String -> List String
sublistBeforeBacktics list =
  List.foldr (\str acc -> if String.contains "```" str then [] else str :: acc) [] list

parseMd : String -> Styles msg -> List (Html msg)
parseMd markdownFile styles =
    let
        lines = String.split "\n" markdownFile
    in
    remainderFoldr2 (
        \line nextLines rec ->
            case firstOf matchers line of
                H1 -> h1 styles.h1 [text <| String.dropLeft 1 line] :: rec
                H2 -> h2 styles.h2 [text <| String.dropLeft 2 line] :: rec
                H3 -> h3 styles.h3 [text <| String.dropLeft 3 line] :: rec
                H4 -> h4 styles.h4 [text <| String.dropLeft 4 line] :: rec
                CODE -> pre [] [ code [] [text <| String.join "\n" <| sublistBeforeBacktics <| nextLines] ] :: rec
                LIST -> ul styles.ul (parseListTag <| (line :: nextLines)) :: rec
                P -> p styles.p [text line] :: rec
                _ -> rec
    ) (\t -> not <| String.contains "```" t) sublistAfterBacktics [] lines

firstOf : List (String -> Maybe MarkdownTag) -> String -> MarkdownTag
firstOf checkers elem =
    case checkers of
        [] -> P
        x::xs -> case x elem of
                    Just res -> res
                    Nothing -> firstOf xs elem
