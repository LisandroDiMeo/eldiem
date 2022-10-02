module Components.Navbar exposing (navbar, NavbarTab)

import Html exposing (Attribute, Html)
import Html exposing (nav, ul, li, img, text, p, div, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (foldr)

type alias NavbarTab msg = {
    title : String,
    imageResource : String,
    onPressed : Maybe msg }


ulStyle : List (Attribute msg)
ulStyle = [style "list-style-type" "none", style "overflow" "hidden"]

navbarContainer : List (Html msg) -> Html msg
navbarContainer = (ul ulStyle)

navbar : List (NavbarTab msg) -> Html msg
navbar tabsInformation = nav [] [navbarContainer (foldr (\tabInfo navigationTabs -> (navbarTab [] tabInfo) :: navigationTabs) [] tabsInformation)]

navbarTab : List (Attribute msg) -> NavbarTab (msg) -> Html msg
navbarTab attributes tabInfo = 
    let image = div [style "display" "inline-flex"] [
                img [src ("assets/"++tabInfo.imageResource), width 32, height 32, style "display" "block", style "padding" "12px 14px"] [],
                a [href tabInfo.title] [p [] [text tabInfo.title]]
            ]
    in 
    case tabInfo.onPressed of
        Just m -> li ([ style "float" "left", onClick m ] ++ attributes) [image]
        Nothing -> li (style "float" "left" :: attributes) [image]