module Commons.EncodeMaybeString exposing (..)

import Json.Encode as Encode

encodeMaybeString : Maybe String -> Encode.Value
encodeMaybeString maybeString =
    case maybeString of
        Nothing ->
            Encode.null

        Just string ->
            Encode.string string