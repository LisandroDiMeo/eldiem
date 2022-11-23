module Page.Posts exposing (..)

import Html exposing (..)
import Commons.Zip exposing (zip)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)

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
        references: List String,
        images: List (Maybe String)
    }

type Model = Failure | Loading | Success Post

-- VIEW 

view : Model -> Html msg
view model = 
    div [style "padding" "12px 24px 12px 24px"] (viewPost model)

viewPost : Model -> List (Html msg)
viewPost model =
    let postBody = List.map (\(content, image) -> 
            case image of
                Nothing -> p [] [text content]
                Just s -> div [] [p [] [text content], img [src s, width 128, height 128, style "display" "block", style "padding" "12px 14px"] []]) 
    in 
    case model of 
        Failure -> [ p [] [text "That post doesn't exist!🤯"] ]
        Loading -> [p [] [text "Loading... 🔄"]]
        Success post -> ([
            h2 [] [text post.title],
            p [] [i [] [text post.summary]],
            p [] [text post.date]
            ] ++ (postBody (zip post.content post.images)) ++ [ p [] [ img [src "src/assets/link.png", width 16, height 16, style "padding-right" "8px"] [], text "Share it!" ] ])

-- UPDATE 

type Msg = OnShareButtonPressed | GotPostWithId (Result Http.Error Post) -- | OnNextPostButton Int

update : Msg -> Model -> (Model, Cmd msg)
update msg model = 
    case msg of 
        OnShareButtonPressed -> (model, Cmd.none)
        GotPostWithId result -> 
            case result of 
                Ok post -> (Success post, Cmd.none)
                Err _ -> (Failure, Cmd.none)
        --OnNextPostButton id -> (Loading, getPostWithId id)

getPostWithId : Int -> Cmd Msg
getPostWithId id = Http.get
    { url = "src/posts/long/post"++(postIdToRealId id)++".json"
    , expect = Http.expectJson GotPostWithId (postDecoder)
    }

postIdToRealId : Int -> String
postIdToRealId id = String.fromInt id |> (\rid -> if (String.length rid < 3) then (if (String.length rid) == 1 then "00" else "0") ++ rid else rid) 

postDecoder : Decoder Post
postDecoder = 
    map7 Post
        (field "title" string)
        (field "summary" string)
        (field "content" (Json.Decode.list string))
        (field "date" string)
        (field "id" int)
        (field "references" (Json.Decode.list string))
        (field "images" (Json.Decode.list (maybe string)))

