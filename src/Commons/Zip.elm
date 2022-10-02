module Commons.Zip exposing (..)

zip : List a -> List b -> List (a,b)
zip l1 l2 = List.map2 Tuple.pair l1 l2