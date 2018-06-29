module Utils exposing (..)


import Regex

type Hex = Hex String

{-| -}
isHex : String -> Bool
isHex =
    Regex.contains (Regex.regex "^((0[Xx]){1})?[0-9a-fA-F]+$")


toHex : String -> Hex
toHex str = 


{-| -}
add0x : String -> String
add0x str =
    if String.startsWith "0x" str || String.startsWith "0X" str then
        str
    else
        "0x" ++ str


{-| -}
remove0x : String -> String
remove0x str =
    if String.startsWith "0x" str || String.startsWith "0X" str then
        String.dropLeft 2 str
    else
        str
