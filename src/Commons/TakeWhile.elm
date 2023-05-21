module Commons.TakeWhile exposing (..)

takeWhile: (String -> Bool) -> String -> String
takeWhile cond s =
    let c = String.left 1 s
    in
    if cond c then
        c
    else
       c ++ (takeWhile cond <| String.dropLeft 1 s)

takeWhileExclusive: (String -> Bool) -> String -> String
takeWhileExclusive cond s =
    let c = String.left 1 s
    in
    if cond c then
        ""
    else
       c ++ (takeWhileExclusive cond <| String.dropLeft 1 s)

