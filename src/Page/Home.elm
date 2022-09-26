module Page.Home exposing
  ( Model
  , init
  , Msg
  , update
  , view
  , LatestPost, latestPosts
  )

import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL

type alias Model = 
    {
        title: String,
        latestsPosts : List (LatestPost)
    }

type alias LatestPost = { title: String, shortText: String, thumbnailResource: String }

init : String -> List LatestPost -> Model
init title latestsPosts = (Model title latestsPosts)

-- UPDATE
type Msg 
    = OnLatestPostPressed

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        OnLatestPostPressed ->
            (model, Cmd.none)

-- VIEW 

view : Model -> Html msg
view model = 
    let posts = List.map (\lp -> div [] [
                h1 [] [text lp.title],
                p [] [text lp.shortText],
                img [src lp.thumbnailResource, width 64, height 64, style "display" "block", style "padding" "12px 14px"] []
            ]) model.latestsPosts
    in 
    div [] posts

-- latests posts
latestPosts : List LatestPost
latestPosts = [
        LatestPost "Welcome" "This is a testing post, nothing much to look here..." "lisa.jpeg",
        LatestPost "Welcome" "This is a testing post, nothing much to look here..." "lisa.jpeg",
        LatestPost "Welcome" "This is a testing post, nothing much to look here..." "lisa.jpeg"
    ]
