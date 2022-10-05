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
            div [] (parseAbout model.content)
        ]

aboutMe = "I'm finishing my M.Sc. in Computer Science at Faculty of Natural and Exacts Sciences, in Universidad de Buenos Aires."
        ++ "%Also I'm beign part of a research internship in LAFHIS, where I'm working with Automatic Testing Generation for Android Apps using Genetic Algorithms."
        ++ "%Additionaly I'm working at Wolox part of Accenture as an Android Lead Developer."
    

parseAbout : String -> List (Html msg)
parseAbout rawAboutMe = 
    let l = split "%" rawAboutMe
    in
    List.map (\line -> p [] [text line]) l
