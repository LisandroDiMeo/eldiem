module Test exposing (..)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)

main : Html a
main =
    span [ class "welcome-message" ] [ text (sayHello Anonymous) ]


type User = Regular String Int | Visitor String | Anonymous

sayHello : User -> String
sayHello usr = 
    case usr of
        Regular name age -> name ++ " 00 "
        Visitor name -> name
        default -> "NO IMPL"
