module Web3.Utils exposing (..)

import Result
import Web3.Types exposing (Error)
import Hex
import Char
import BigInt exposing (BigInt)
import List.Extra
import Result.Extra

{-| Used to transform a hexadecimal-encoded quantity into an Elm integer.

Will return Err if:

- hex string does not start with '0x'
- hex string contains non-hexadecimal values (0-9, a-f; uppercase A-F are not recognized!)

TODO Will probably be deprecated, since using BigInts is safer.
 -}
hexQuantityToInt : String -> Result Error Int
hexQuantityToInt quantity =
    quantity
        |> stripPrefix0x
        |> Result.andThen Hex.fromString
        |> Result.mapError Web3.Types.ResultParseError


{-| Used to transform a hexadecimal-encoded quantity into a BigInt.

Will return Err if:

- hex string does not start with '0x'
- hex string contains non-hexadecimal values (0-9, a-f; uppercase A-F are not recognized!)

 -}
hexQuantityToBigInt : String -> Result Error BigInt
hexQuantityToBigInt quantity =
    if not (String.startsWith "0x" quantity) then
        Result.Err (Web3.Types.ResultParseError "Hexadecimal quantities are expected to begin with 0x")
    else
        quantity
            |> BigInt.fromString
            |> Result.fromMaybe (Web3.Types.ResultParseError "Could not parse response as hexadecimal BigInt")


{-| Used to transform quantities (BigInteger numbers representing amounts, balances, timestamps, block numbers etc) into their hexadecimal format.

Will return Err if:

- hex string does not start with '0x'
- hex string contains non-hexadecimal values (0-9, a-f; uppercase A-F are not recognized!)

TODO Will probably be deprecated, since using BigInts is safer.
-}
intToHexQuantity : Int -> String
intToHexQuantity quantity =
    quantity
        |> Hex.toString
        |> prefix0x

{-| Used to transform quantities (BigInteger numbers representing amounts, balances, timestamps, block numbers etc) into their hexadecimal format.

TODO negative numbers! (non-encodable in Ethereum)

### Example

    bigIntToHexQuantity (BigInt.fromInt 10) == "0xa"
 -}
bigIntToHexQuantity : BigInt -> String
bigIntToHexQuantity quantity =
    quantity
        |> BigInt.toHexString
        |> prefix0x


{-| Used to transform unformatted data in 'String' format,
(which might represent byte arrays, account addresses, hashes, bytecode arrays)
into their hexadecimal format.


### Example:

    unformattedDataToHexString "foo" == "0x666f6f"

-}
unformattedDataToHexString : String -> String
unformattedDataToHexString input =
    input
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""
        |> prefix0x


{-| Helper function to ensure hexadecimal strings start with '0x'.
-}
prefix0x : String -> String
prefix0x value =
    "0x" ++ value


{-| Helper function to ensure hexadecimal input starts with '0x'.

If it does not, an Error is returned.
If it does, this prefix is stripped, and Ok String is returned.
-}
stripPrefix0x : String -> Result String String
stripPrefix0x str =
    if not (String.startsWith "0x" str) then
        Result.Err "Hexadecimal data is expected to begin with 0x"
    else
        str
            |> String.dropLeft 2
            |> Result.Ok


{-| Turns a hexadecimal string like "0x666f6f" back into its String equivalent (in this case: "foo").

Returns error if:

  - hexString does not start with '0x'
  - hexString contains non-hexadecimal characters (outside the 0-9, a-f range; capital A-F is not recognized!)


### Example:

    hexStringToUnformattedData "0x666f6f" == Ok "foo"

-}
hexStringToUnformattedData : String -> Result Error String
hexStringToUnformattedData input =
    let
        unsafeFun stripped_input =
            stripped_input
                |> String.toList
                |> List.Extra.groupsOf 2
                |> List.map (List.map (String.fromChar) >> String.join "" >> Hex.fromString >> Result.map (Char.fromCode))
                |> Result.Extra.combine
                |> Result.map (String.fromList)
    in
        input
            |> stripPrefix0x
            |> Result.andThen unsafeFun
            |> Result.mapError Web3.Types.ResultParseError
