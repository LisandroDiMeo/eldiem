module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, parse)
import Html exposing (h1, text)
import Components.Navbar exposing (navbar, NavbarTab)
import Page.Home as Home
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
        {title = "Home", imageResource = "image.png", onPressed = Nothing},
        {title = "Posts", imageResource = "image.png", onPressed = Nothing},
        {title = "About", imageResource = "image.png", onPressed = Nothing},
        {title = "Other", imageResource = "image.png", onPressed = Nothing}
    ]

header : Html.Html msg
header = navbar navBarTabs 

wrapperFor : Page -> Browser.Document Msg
wrapperFor page 
    = case page of
        Home home -> Browser.Document "Home" ([header, Home.view home])
        NotFound -> Browser.Document "NotFound" ([header])

-- INIT

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  stepUrl url
    { key = key
    , page = Home (Home.Model "Home" Home.latestPosts)
    }
    
stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
    let parser = oneOf [
            route (s "Main.elm") (stepHome model (Home.init "Home" Home.latestPosts, Cmd.none))
            , route (s "Home") (stepHome model (Home.init "Home" Home.latestPosts, Cmd.none))
            ]
    in 
    case parse parser url of 
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
        

stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> (Model, Cmd Msg)
stepHome model (home, cmds) = ( {model | page = Home home}, Cmd.map HomeMsg cmds )

-- ROUTER 

