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

type alias LatestPost = { title: String, shortText: String, thumbnailResource: String, id: Int }

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
                img [src lp.thumbnailResource, width 128, height 128, style "display" "block", style "padding" "12px 14px"] []
            ]) model.latestsPosts
    in 
    div [style "padding" "12px 24px 12px 24px"] posts

-- latests posts
latestPosts : List LatestPost
latestPosts = [
        LatestPost "Espresso is missing views" "Espresso may be lacking of information..." "espresso.png" 1,
        LatestPost "Welcome" "This is a testing post, nothing much to look here..." "lisa.jpeg" 0
    ]
