module Page.About exposing (..)
import Html exposing (..)
import String exposing (split)
import Html.Attributes exposing (..)
import Url.Parser exposing (parse)

-- MODEL

type alias Model =
    { title : String
    , content : String
    }

staticAbout = Model "" aboutMe

init : String -> String -> Model
init title content = Model title content

-- UPDATE

type Msg = OnImagePressed

update : Msg -> Model -> (Model, Cmd msg)
update _ model = (model, Cmd.none)

-- VIEW

view : Model -> Html msg
view model = 
    div [ style "padding" "12px 24px 12px 24px" ] [
            h1 [] [text "About me"],
            div [] (parseAbout model.content),
            div [] [img [src "src/assets/me.jpg", style "width" "512px", alt "Thats me in Bariloche :)"] []]
        ]

aboutMe =     
    "I'm finishing my M.Sc. in Computer Sciences 🖥️ at FCEN,  Universidad de Buenos Aires."
    ++
    "%Currently I'm part of a research internship at LAFHIS, where I'm working with Automatic Testing Generation for Android Apps using Genetic Algorithms 🧬."
    ++
    "%Additionaly I work at Wolox part of Accenture as an Android Lead Developer 🤖."
    ++
    "%Beyond that, I really like music 🎵 (I play the guitar), photography 📷, and nature ⛰️."


parseAbout : String -> List (Html msg)
parseAbout rawAboutMe = 
    let l = split "%" rawAboutMe
    in
    List.map (\line -> p [] [text line]) l
