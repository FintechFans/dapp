module EthAbi.Types
    exposing
        ( Hexstring
        , hexstring
        , hexstringToString
        , Int256
        , int
        , int256
        , int256ToBigInt
        , UInt256
        , uint
        , uint256
        , uint256ToBigInt
        , bytes
        , Bytes
        , bytes32
        , Bytes32
        )

{-| -}

import BigInt exposing (BigInt)
import Result exposing (Result)
import Char
import Hex
import EthAbi.Internal exposing (ensure, Hexstring(..), Bytes32(..), Bytes(..))


type alias Hexstring =
    EthAbi.Internal.Hexstring


{-| Creates a hexadecimal string from some input string.

  - makes sure that it is actually hexadecimal
  - Ensures that it is actually a multiple of 32 bytes (64 hexadecimal characters), because all ABI requests and responses are this way.

Do note, that the extra four bytes (8 hexchars) that are prepended to a request to represent the function signature hash mean that those requests are *not* `EthAbi.Hexstring`'s!

TODO have a special data structure for function calls.

-}
hexstring : String -> Result String Hexstring
hexstring raw_str =
    let
        hexadecimal str =
            String.all Char.isHexDigit str

        multipleOf32Bytes str =
            String.length str % 64 == 0
    in
        raw_str
            |> ensure hexadecimal "Not a hexadecimal string"
            |> Result.andThen (ensure multipleOf32Bytes "String not a multiple of 32 bytes (64 hexadecimal characters)")
            |> Result.map Hexstring


hexstringToString : Hexstring -> String
hexstringToString (Hexstring hexstr) =
    hexstr


type Int256
    = Int256 BigInt


int : Int -> BigInt -> Result String Int256
int len integer =
    let
        appropriateLength len _ =
            len > 0 && len <= 256 && len % 8 == 0
    in
        integer
            |> ensure (appropriateLength len) ("length should be in range 0..256 and be a multiple of 8, but it is " ++ (toString len))
            |> Result.andThen (ensure (integerFits (len - 1)) ("Integer too large to fit in " ++ toString len ++ " bits"))
            |> Result.map (Int256)



-- TODO: Constructor shorthand for every `int<M>`?


int8 =
    int 8


int16 =
    int 16


int32 =
    int 32


int64 =
    int 64


int128 =
    int 128


int256 =
    int 256


int256ToBigInt (Int256 big_int) =
    big_int


type UInt256
    = UInt256 BigInt


uint : Int -> BigInt -> Result String UInt256
uint len integer =
    let
        appropriateLength len _ =
            len > 0 && len <= 256 && len % 8 == 0

        isPositive integer =
            BigInt.gte integer (BigInt.fromInt 0)

        ensurePositive integer =
            if BigInt.gte integer (BigInt.fromInt 0) then
                Ok integer
            else
                Err ("UInt256 called with a negative value: " ++ toString integer)
    in
        integer
            |> ensure (appropriateLength len) ("length should be in range 0..256 and be a multiple of 8, but it is " ++ (toString len))
            |> Result.andThen (ensure isPositive ("UInt256 called with a negative value: " ++ toString integer))
            |> Result.andThen (ensure (integerFits len) ("Unsigned Integer too large to fit in " ++ toString len ++ " bits"))
            |> Result.map (UInt256)



-- TODO: Constructor shorthand for every `uint<M>`?


uint8 =
    uint 8


uint16 =
    uint 16


uint32 =
    uint 32


uint64 =
    uint 64


uint128 =
    uint 128


uint256 =
    uint 256


uint256ToBigInt (UInt256 big_int) =
    big_int


integerFits : Int -> BigInt -> Bool
integerFits len integer =
    let
        len_bits len =
            BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt len)
    in
        BigInt.lte (BigInt.abs integer) (len_bits len)


type alias Bytes32 =
    EthAbi.Internal.Bytes32


fixed_bytes : Int -> String -> Result String Bytes32
fixed_bytes len str =
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


bytes32 =
    fixed_bytes 32


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
bytesFromString : String -> Bytes
bytesFromString str =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""
        |> Bytes
