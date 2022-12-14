module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, parse)
import Html exposing (h1, text)
import Components.Navbar exposing (navbar, NavbarTab)
import Components.Footer exposing (footer)
import Page.Home as Home
import Page.About as About
import Page.Posts as Posts
import Html.Attributes exposing (..)

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
    -- | Other

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

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
    = case page of
        Home home -> Browser.Document "Home" ([header, Home.view home, footer])
        NotFound -> Browser.Document "NotFound" ([header, footer])
        About about -> Browser.Document "About" ([header, About.view about, footer])
        Posts post -> Browser.Document "Post" ([header, Posts.view post, footer])

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
                        Just s -> s
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

update : Msg -> Model -> ( Model, Cmd Msg )
update message model = 
    case message of
        NoOp -> (model, Cmd.none)
        
        LinkClicked urlRequest -> 
            case urlRequest of 
                Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url))

                Browser.External href -> (model, Nav.load href)

        UrlChanged url -> stepUrl url model

        HomeMsg msg -> 
            case model.page of
                Home home -> stepHome model (Home.update msg home)
                _ -> (model, Cmd.none)
        
        AboutMsg msg -> 
            case model.page of 
                About about -> stepAbout model (About.update msg about)
                _ -> (model, Cmd.none)

        PostsMsg msg -> 
            case model.page of
                Posts post -> stepPost model (Posts.update msg post)
                _ -> (model, Cmd.none)

stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> (Model, Cmd Msg)
stepHome model (home, cmds) = ( {model | page = Home home}, Cmd.map HomeMsg cmds )

stepAbout : Model -> ( About.Model, Cmd About.Msg ) -> (Model, Cmd Msg)
stepAbout model (about, cmds) = ( { model | page = About about }, Cmd.map AboutMsg cmds )

stepPost : Model -> ( Posts.Model, Cmd Posts.Msg ) -> (Model, Cmd Msg)
stepPost model (post, cmds) = ( { model | page = Posts post }, Cmd.map PostsMsg cmds )

-- ROUTER 

