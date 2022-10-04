module Page.Home exposing
  ( Model (..)
  , init
  , Msg
  , update
  , view
  , LatestPost
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)

-- MODEL

type Model = Failure | Loading | Success (List (LatestPost))

type alias LatestPost = { title: String, shortText: String, thumbnailResource: String, id: Int }

init : () -> (Model, Cmd Msg)
init _ = (Loading, getLatestsPosts)

-- UPDATE
type Msg 
    = OnLatestPostPressed | GotLatestsPosts (Result Http.Error LatestPost)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        OnLatestPostPressed ->
            (model, Cmd.none)
        GotLatestsPosts result -> 
            case result of 
                Ok lp -> (Success [lp] , Cmd.none)
                Err _ -> (Failure, Cmd.none)

-- VIEW 

view : Model -> Html msg
view model = 
    div [style "padding" "12px 24px 12px 24px"] (viewLatestsPosts model)

viewLatestsPosts : Model -> List (Html msg)
viewLatestsPosts model = 
    case model of 
        Failure -> [p [] [text "Posts not found :/"]]
        Loading -> [p [] [text "Loading..."]]
        Success latestposts -> List.map (\lp -> div [] [
                h1 [] [text lp.title],
                p [] [text lp.shortText],
                img [src lp.thumbnailResource, width 128, height 128, style "display" "block", style "padding" "12px 14px"] []
            ]) latestposts

-- Fetch Posts

getLatestsPosts : Cmd Msg
getLatestsPosts = Http.get
    { url = "src/posts/short/post001.json"
    , expect = Http.expectJson GotLatestsPosts latestPostDecoder
    }

latestPostDecoder : Decoder LatestPost
latestPostDecoder =
  map4 LatestPost
    (field "title" string)
    (field "shortText" string)
    (field "thumbnailResource" string)
    (field "id" int)