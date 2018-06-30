module Web3.Utils exposing (..)

import Result
import Web3.Types exposing (Error, Address(..), UnformattedData)
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
hexQuantityToInt : String -> Result String Int
hexQuantityToInt quantity =
    quantity
        |> stripPrefix0x
        |> Result.andThen Hex.fromString


{-| Used to transform a hexadecimal-encoded quantity into a BigInt.

Will return Err if:

  - hex string does not start with '0x'
  - hex string contains non-hexadecimal values (0-9, a-f; uppercase A-F are not recognized!)

-}
hexQuantityToBigInt : String -> Result String BigInt
hexQuantityToBigInt quantity =
    if not (String.startsWith "0x" quantity) then
        Result.Err "Hexadecimal quantities are expected to begin with 0x"
    else
        quantity
            |> BigInt.fromString
            |> Result.fromMaybe "Could not parse response as hexadecimal BigInt"


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
unformattedDataToHexString : UnformattedData -> String
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

Returns Err if:

  - hexString does not start with '0x'
  - hexString contains non-hexadecimal characters (outside the 0-9, a-f range; capital A-F is not recognized!)


### Example:

    hexStringToUnformattedData "0x666f6f" == Ok "foo"

-}
hexStringToUnformattedData : String -> Result String UnformattedData
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


{-| Attempts to turn a hexadecimal string into an Ethereum Address

Returns Err if:

  - Not hexadecimal
  - More than 20 bytes long.

(If the input address is shorter than 20 bytes, NUL-character bytes are added to the left to make the address 20 bytes. This is required to parse addresses like `"0x"`, `"0x0"`, `"0x10"` etc correctly)

-}
hexStringToAddress : String -> Result String Address
hexStringToAddress input =
    let
        isAddress str =
            String.length str <= 20
    in
        input
            |> hexStringToUnformattedData
            |> Result.andThen
                (\x ->
                    if isAddress x then
                        Ok (Address (String.padLeft 20 '\0' x))
                    else
                        Err ("Passed hexadecimal string was not an address (not 20 bytes long)")
                )
