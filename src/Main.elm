port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Json.Encode as Encode exposing (string)
import Maybe exposing (withDefault)
import Url
import Url.Builder
import Url.Parser exposing (Parser, (</>), int, oneOf, s, parse)
import Html
import Components.Navbar exposing (navbar, NavbarTab)
import Components.Footer exposing (footer)
import Page.LatestPosts as LatestPosts exposing (Msg(..))
import Page.About as About exposing (staticAbout)
import Page.Posts as Posts exposing (Model(..), Msg(..), onShareButtonPressed)

-- MAIN

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }

-- MODEL

type alias Model = 
    {
        key: Nav.Key,
        page: Page
    }

type Page 
    = Home About.Model
    | NotFound
    | Posts Posts.Model
    | LatestPost LatestPosts.Model
    -- | Other

port sendMessage : List (String) -> Cmd msg
port messageReceiver : (List(String) -> msg) -> Sub msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = messageReceiver LinkCopied

-- VIEW

view : Model -> Browser.Document Msg
view model = wrapperFor model.page

navBarTabs : List (NavbarTab msg)
navBarTabs = [
        {title = "Home", imageResource = "finder.png", onPressed = Nothing},
        {title = "LatestPosts", imageResource = "floppydisk.png", onPressed = Nothing},
        {title = "Other", imageResource = "misc.png", onPressed = Nothing}
    ]

header : Html.Html msg
header = navbar navBarTabs

wrapperFor : Page -> Browser.Document Msg
wrapperFor page
    =
    case page of
        Home about -> Browser.Document "Home" ([header, About.view about, footer])
        NotFound -> Browser.Document "NotFound" ([header, footer])
        LatestPost latestPosts -> Browser.Document "LatestPosts" ([header, LatestPosts.view latestPosts |> Html.map LatestPostsMsg, footer])
        Posts post -> Browser.Document "Post" ([header, Posts.view post |> Html.map PostsMsg, footer])

-- INIT

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  stepUrl url
    { key = key
    , page = Home staticAbout
    }

stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
    let parser = oneOf [
            route Url.Parser.top (stepHome model (About.init "Home" About.aboutMe, Cmd.none))
            , route (s "Home") (stepHome model (About.init "Home" About.aboutMe, Cmd.none))
            , route (s "LatestPosts") (stepLatestPosts model (LatestPosts.init ()))
            , route (s "Posts" </> int) (\id -> stepPost model (Posts.init (id)))
            , route (s "Posts") (stepPost model (Posts.init (1)))
            ]
        fakeUrl = {
                    url | path = case url.fragment of
                        Just str -> str
                        Nothing -> ""
                }
    in
    case parse parser fakeUrl of
        Just answer -> answer
        Nothing -> ( { model | page = NotFound }, Cmd.none )

route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Url.Parser.map handler parser

-- UPDATE

type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg About.Msg
    | LatestPostsMsg LatestPosts.Msg
    | PostsMsg Posts.Msg
    | LinkCopied (List String)

postUrlWithId : String -> Maybe Url.Url
postUrlWithId postId = Url.fromString <| absoluteUrl postId

--check how to remove localhost hardcoded
absoluteUrl : String -> String
absoluteUrl postId = "http://localhost:8080" ++ (Url.Builder.absolute ["#","Posts",postId] [])

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp -> (model, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href -> (model, Nav.load href)

        UrlChanged url ->
            stepUrl url model

        HomeMsg msg ->
            case model.page of
                            Home about -> stepHome model (About.update msg about)
                            _ -> (model, Cmd.none)

        LatestPostsMsg msg ->
            case model.page of
                LatestPost home ->
                    case msg of
                        GotLatestPosts _ -> stepLatestPosts model (LatestPosts.update msg home)
                        OnLatestPostPressed postId -> case postUrlWithId <| postId of
                                                            Just postUrl -> stepPost model (Posts.init <| Maybe.withDefault 0 <| String.toInt postId) -- update (LinkClicked (Browser.Internal postUrl)) model
                                                            Nothing -> (model, Cmd.none)
                _ -> (model, Cmd.none)

        PostsMsg msg ->
            case model.page of
                Posts post ->
                    let d = Debug.log "MainPostLog" post
                    in
                    case msg of
                        GotPostWithId _ -> stepPost model (Posts.update msg post)
                        OnShareButtonPressed (postContent, postId) -> (model, sendMessage <| y postContent postId <| (Posts.update msg (onShareButtonPressed post)))
                            -- (model, sendMessage <| encodePost <| Tuple.first <| (Posts.update msg (onShareButtonPressed post)))
                _ -> (model, Cmd.none)

        LinkCopied postInformation ->
            let postInfo = List.head postInformation |> withDefault "" |> String.toInt |> withDefault 0
                postContent = List.tail postInformation |> withDefault [""] |> List.head |> withDefault ""
                shareMsg = OnShareButtonPressed (postContent, postInfo)
                shareMainModel = ShareButtonPressed (postContent, postInfo)
                shareCmd = Posts.update shareMsg shareMainModel
            in
            stepPost model shareCmd

y : String -> Int -> (Posts.Model, Cmd Posts.Msg) -> List String
y post id _ = [String.fromInt id, post]

stepHome : Model -> ( About.Model, Cmd About.Msg ) -> (Model, Cmd Msg)
stepHome model (home, cmds) = ( {model | page = Home home}, Cmd.map HomeMsg cmds )

stepLatestPosts : Model -> ( LatestPosts.Model, Cmd LatestPosts.Msg ) -> (Model, Cmd Msg)
stepLatestPosts model (home, cmds) = ( {model | page = LatestPost home}, Cmd.map LatestPostsMsg cmds )

stepPost : Model -> ( Posts.Model, Cmd Posts.Msg ) -> (Model, Cmd Msg)
stepPost model (post, cmds) = ( { model | page = Posts post }, Cmd.map PostsMsg cmds )

-- ROUTER 

