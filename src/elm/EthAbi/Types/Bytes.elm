module EthAbi.Types.Bytes
    exposing
        ( Bytes32
        , Bytes
        , static_bytes
        , bytes
        , bytes1
        , bytes32
        , fromString
        )

import Char
import Hex
import EthAbi.Internal exposing (Bytes32(..), Bytes(..), ensure)


type alias Bytes32 =
    EthAbi.Internal.Bytes32


static_bytes : Int -> String -> Result String Bytes32
static_bytes len str =
    let
        ensureLenIsInRange len str =
            if len > 0 && len <= 32 then
                Ok str
            else
                Err ("Bytes length should be in range 0..32, but it is " ++ (toString len))

        -- TODO
        ensureStringFits str =
            if String.length str <= len then
                Ok (Bytes32 str)
            else
                Err "String is too large to fit in a Bytes32."
    in
        str
            |> ensureLenIsInRange len
            |> Result.andThen ensureStringFits



-- TODO: Constructor shorthand for every `bytes<M>`?


bytes1 =
    static_bytes 1


bytes32 =
    static_bytes 32


bytes32ToString : Bytes32 -> String
bytes32ToString (Bytes32 str) =
    str


type alias Bytes =
    EthAbi.Internal.Bytes


{-| Creates a new `Bytes` object from an existing hexadecimal string that is interpretable as a byte-string:

  - Only allowed to contain hexadecimal characters
  - String should have an even length, since otherwise it cannot possibly store bytes.

-}
bytes : String -> Result String Bytes
bytes bstr =
    let
        isHexadecimal str =
            String.all Char.isHexDigit str

        isRepresentableAsBytes str =
            String.length bstr % 2 == 0
    in
        bstr
            |> ensure isHexadecimal "Not a hexadecimal string, cannot interpret as already-encoded bytes"
            |> Result.andThen (ensure isRepresentableAsBytes "Hexadecimal string has an odd number of characters, so it cannot possibly store bytes")
            |> Result.map Bytes


{-| Encodes an arbitrary string into a `Bytes` string,

by interpreting every char as number between [0..255] and writing that as
two hexdigits [00..ff]

-}
fromString : String -> Bytes
fromString str =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""
        |> Bytes
