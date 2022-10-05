module Components.Footer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

footer : Html msg
footer = Html.footer [class "content-padding"] [
    div [] [
            div [class "footer-icons"] [p [] [text "Find me at: "], github, linkedin],
            br [] [],
            br [] [],
            small [] [text "No rights reserved, just be aware that identity theft is not a joke, millions of families suffer it every year. "]
            ]
    ]

github = a [href "https://github.com/lisandroDiMeo/", target "_blank"] [img [src "src/assets/pxgithub.png", class "github-icon"] []]
linkedin = a [href "https://www.linkedin.com/in/lisandrodimeo/", target "_blank"] [img [src "src/assets/pxlinkedin.png", class "linkedin-icon"] []]