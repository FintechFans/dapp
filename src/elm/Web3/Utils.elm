module Web3.Utils exposing (..)

import Result
import Web3.Types exposing (Error)
import Hex


{-| TODO Bigint? -}
hexQuantityToInt : String -> Result Error Int
hexQuantityToInt quantity =
    quantity
        |> String.dropLeft 2
        |> Hex.fromString
        |> Result.mapError Web3.Types.ResultParseError
