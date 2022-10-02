module Page.Posts exposing (..)
import Html exposing (..)
import Commons.Zip exposing (zip)
import Html.Attributes exposing (..)

-- INIT

init : Post -> Post
init p = p

examplePost : Post
examplePost = Post "Lipsum" "Example" [] "25/04/1999" -1 [] []

-- MODEL

type alias Post = 
    {
        title: String,
        summary: String,
        content: List String, 
        date: String,
        id: Int,
        references: List String,
        images: List (Maybe String)
    }

-- VIEW 

view : Post -> Html msg
view post = 
    let postBody = List.map (\(content, image) -> 
            case image of
                Nothing -> p [] [text content]
                Just s -> div [] [p [] [text content], img [src s, width 128, height 128, style "display" "block", style "padding" "12px 14px"] []]) (zip post.content post.images)
    in
    div [style "padding" "12px 24px 12px 24px"] ([
            h2 [] [text post.title],
            p [] [i [] [text post.summary]],
            p [] [text post.date]
        ] ++ postBody ++ [ p [] [ img [src "assets/link.png", width 16, height 16, style "padding-right" "8px"] [], text "Share it!" ] ])

-- UPDATE 

type Msg = OnShareButtonPressed

update : Msg -> Post -> (Post, Cmd msg)
update msg post = 
    case msg of 
        OnShareButtonPressed -> (post, Cmd.none)

