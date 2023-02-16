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

type Model = Failure | Loading | Success (String, Int) | ShareButtonPressed (String, Int)

-- VIEW 

view : Model -> Html Msg
view model = 
    div [style "padding" "12px 24px 12px 24px"] (viewPost model)

viewPost : Model -> List (Html Msg)
viewPost model =
    case model of 
        Failure -> [ p [] [text "That post doesn't exist (yet)!🤯"] ]
        Loading -> [p [] [text "Loading... 🔄"]]
        Success (post, id) ->
            let lines = String.split "\n" post
            in
            buildPostBody lines False id
        ShareButtonPressed (post, id) ->
            let lines = String.split "\n" post
            in
            buildPostBody lines True id

buildPostBody : List (String) -> Bool -> Int -> List (Html Msg)
buildPostBody post linkCopied postId =
    let postContentWithImages = parseMd post customStyles
        shareButtonText = if linkCopied then "Copied to clipboard!" else "Share it!"
    in
    postContentWithImages
    ++ [ p [] [
                img [src "src/assets/link.png"
                , onClick (OnShareButtonPressed ((String.join "\n" post), postId))
                , width 16
                , height 16
                , style "padding-right" "8px"
                ] [], text shareButtonText ] ]
    ++ pagination postId


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

type Msg = OnShareButtonPressed (String, Int) | GotPostWithId (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    let d0 = Debug.log "Test" msg
        d1 = Debug.log "test" model
    in
    case msg of 
        OnShareButtonPressed post ->
            (ShareButtonPressed post, Cmd.none)
        GotPostWithId result -> 
            case result of 
                Ok post -> (Success (post,2), Cmd.none)
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
