module Page.Posts exposing (..)

import Commons.ContentParser exposing (contentParser, customStyles, parseMd)
import Commons.EncodeMaybeString exposing (encodeMaybeString)
import Html exposing (..)
import Commons.Zip exposing (zip)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Encode as Encode

-- INIT

init : Int -> (Model, Cmd Msg)
init id = (Loading, getPostWithId id)

-- MODEL

type alias Post = 
    {
        title: String,
        summary: String,
        content: List String, 
        date: String,
        id: Int,
        references: List String
    }

type Model = Failure | Loading | Success String | ShareButtonPressed String

-- VIEW 

view : Model -> Html Msg
view model = 
    div [style "padding" "12px 24px 12px 24px"] (viewPost model)

viewPost : Model -> List (Html Msg)
viewPost model =
    let postBody  = (\postContent -> contentParser postContent [] [width 128, height 128, style "display" "block", style "padding" "12px 14px"])
    in
    case model of 
        Failure -> [ p [] [text "That post doesn't exist (yet)!🤯"] ]
        Loading -> [p [] [text "Loading... 🔄"]]
        Success post ->
            let lines = String.split "\n" post
            in
            buildPostBody lines False
        ShareButtonPressed post ->
            let lines = String.split "\n" post
            in
            buildPostBody lines True

buildPostBody : List (String) -> Bool -> List (Html Msg)
buildPostBody post linkCopied =
    let postContentWithImages = parseMd post customStyles
        shareButtonText = if linkCopied then "Copied to clipboard!" else "Share it!"
    in
    postContentWithImages
    ++ [ p [] [
                img [src "src/assets/link.png"
                , onClick (OnShareButtonPressed "")
                , width 16
                , height 16
                , style "padding-right" "8px"
                ] [], text shareButtonText ] ]
    ++ pagination 1


pagination : Int -> List (Html msg)
pagination postId = [
    div [style "display" "inline-flex"] <| if hasPreviousPosts postId then dualPagination postId else singlePagination postId
    ]

dualPagination : Int -> List (Html msg)
dualPagination postId = [
    a [href <| "#/Posts/" ++ String.fromInt (postId - 1), style "text-decoration" "none"] [text "<prev"] ,
    a [href <| "#/Posts/" ++ String.fromInt (postId + 1), style "text-decoration" "none"] [text "next>"]
    ]

singlePagination : Int -> List (Html msg)
singlePagination postId = [
    a [href <| "#/Posts/" ++ String.fromInt (postId + 1), style "text-decoration" "none"] [text "next>"]
    ]


hasPreviousPosts : Int -> Bool
hasPreviousPosts postId = if postId <= 1 then False else True

-- UPDATE 

type Msg = OnShareButtonPressed String | GotPostWithId (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of 
        OnShareButtonPressed post ->
            (ShareButtonPressed post, Cmd.none)
        GotPostWithId result -> 
            case result of 
                Ok post -> (Success post, Cmd.none)
                Err _ -> (Failure, Cmd.none)
        --OnNextPostButton id -> (Loading, getPostWithId id)

getPostWithId : Int -> Cmd Msg
getPostWithId id = Http.get
    { url = "src/posts/long/post"++(postIdToRealId id)++".txt"
    , expect = Http.expectString GotPostWithId
    }

postIdToRealId : Int -> String
postIdToRealId id = String.fromInt id |> (\rid -> if (String.length rid < 3) then (if (String.length rid) == 1 then "00" else "0") ++ rid else rid) 

onShareButtonPressed : Model -> Model
onShareButtonPressed postToShareModel =
    case postToShareModel of
        Success post -> ShareButtonPressed post
        _ -> postToShareModel
