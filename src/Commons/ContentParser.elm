module Commons.ContentParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (contains)

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