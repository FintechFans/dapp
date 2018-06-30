module Web3.Utils exposing (..)

import Result
import Web3.Types exposing (Error)
import Hex
import Char


{-| TODO Bigint?
-}
hexQuantityToInt : String -> Result Error Int
hexQuantityToInt quantity =
    quantity
        |> String.dropLeft 2
        |> Hex.fromString
        |> Result.mapError Web3.Types.ResultParseError


intToHexQuantity : Int -> String
intToHexQuantity quantity =
    quantity
        |> Hex.toString
        |> (\res -> "0x" ++ res)


toHexString : String -> String
toHexString input =
    input
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""
        |> (\value -> "0x" ++ value)
