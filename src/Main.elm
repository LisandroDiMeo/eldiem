port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Commons.EncodeMaybeString exposing (encodeMaybeString)
import Json.Decode as Decode
import Json.Encode as Encode exposing (string)
import Url
import Url.Builder
import Url.Parser exposing (Parser, (</>), int, oneOf, s, parse)
import Html
import Components.Navbar exposing (navbar, NavbarTab)
import Components.Footer exposing (footer)
import Page.Home as Home exposing (Msg(..))
import Page.About as About
import Page.Posts as Posts exposing (Model(..), Msg(..), encodePost, onShareButtonPressed, postDecoder)

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
    = Home Home.Model
    | NotFound
    | About About.Model
    | Posts Posts.Model
    -- | Other

port sendMessage : (Encode.Value) -> Cmd msg
port messageReceiver : (Encode.Value -> msg) -> Sub msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = messageReceiver LinkCopied

-- VIEW

view : Model -> Browser.Document Msg
view model = wrapperFor model.page

navBarTabs : List (NavbarTab msg)
navBarTabs = [
        {title = "Home", imageResource = "finder.png", onPressed = Nothing},
        {title = "Posts", imageResource = "floppydisk.png", onPressed = Nothing},
        {title = "About", imageResource = "coffee.png", onPressed = Nothing},
        {title = "Other", imageResource = "misc.png", onPressed = Nothing}
    ]

header : Html.Html msg
header = navbar navBarTabs 

wrapperFor : Page -> Browser.Document Msg
wrapperFor page 
    =
    case page of
        Home home -> Browser.Document "Home" ([header, Home.view home |> Html.map HomeMsg, footer])
        NotFound -> Browser.Document "NotFound" ([header, footer])
        About about -> Browser.Document "About" ([header, About.view about, footer])
        Posts post -> Browser.Document "Post" ([header, Posts.view post |> Html.map PostsMsg, footer])

-- INIT

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  stepUrl url
    { key = key
    , page = Home (Home.Loading)
    }
    
stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
    let parser = oneOf [
            route Url.Parser.top (stepHome model (Home.init ()))
            , route (s "Home") (stepHome model (Home.init ()))
            , route (s "About") (stepAbout model (About.init "About" About.aboutMe, Cmd.none))
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
    | HomeMsg Home.Msg
    | AboutMsg About.Msg
    | PostsMsg Posts.Msg
    | LinkCopied Encode.Value

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
                Home home ->
                    case msg of
                        GotLatestsPosts _ -> stepHome model (Home.update msg home)
                        OnLatestPostPressed postId -> case postUrlWithId <| postId of
                                                            Just postUrl -> stepPost model (Posts.init <| Maybe.withDefault 0 <| String.toInt postId) -- update (LinkClicked (Browser.Internal postUrl)) model
                                                            Nothing -> (model, Cmd.none)
                _ -> (model, Cmd.none)

        AboutMsg msg -> 
            case model.page of 
                About about -> stepAbout model (About.update msg about)
                _ -> (model, Cmd.none)

        PostsMsg msg ->
            case model.page of
                Posts post ->
                    let d = Debug.log "MainPostLog" post
                    in
                    case msg of
                        GotPostWithId _ -> stepPost model (Posts.update msg post)
                        OnShareButtonPressed _ ->
                            (model, sendMessage <| encodePost <| Tuple.first <| (Posts.update msg (onShareButtonPressed post)))
                _ -> (model, Cmd.none)
        LinkCopied s ->
            let post = Decode.decodeValue postDecoder s
            in
            case post of
                Ok decodedPost -> stepPost model (Posts.update (OnShareButtonPressed decodedPost) <| ShareButtonPressed decodedPost)
                Err _ -> (model, Cmd.none)


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> (Model, Cmd Msg)
stepHome model (home, cmds) = ( {model | page = Home home}, Cmd.map HomeMsg cmds )

stepAbout : Model -> ( About.Model, Cmd About.Msg ) -> (Model, Cmd Msg)
stepAbout model (about, cmds) = ( { model | page = About about }, Cmd.map AboutMsg cmds )

stepPost : Model -> ( Posts.Model, Cmd Posts.Msg ) -> (Model, Cmd Msg)
stepPost model (post, cmds) = ( { model | page = Posts post }, Cmd.map PostsMsg cmds )

-- ROUTER 

