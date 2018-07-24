module EthAbi.Encode exposing (encode)

import BigInt exposing (BigInt)
import Char
import Hex
import Result.Extra
import List.Extra

import EthAbi.Types exposing (hexstring, Int256, UInt256, int256ToBigInt, uint256ToBigInt)
import EthAbi.Internal exposing (ensure, Hexstring, Bytes32(..))

-- for internal use only
type alias Hexstring = String

{-| An Encoder turns an 'a' into a pair of Hexstrings.
The final 'encode' function finally concatenates these two, but it is important for them to be kept separate because if multiple encoders run one-after-the-other, content needs to be added in-between the two.
 -}
type alias Encoder a = a -> (Hexstring, Hexstring)-> (Hexstring, Hexstring)

encode : Encoder a -> a -> EthAbi.Types.Hexstring
encode encoder data =
    let
        concatTuple (a, b) = a ++ b
    in
            partialEncode encoder data ("", "")
                      |> concatTuple
                      |> EthAbi.Internal.Hexstring

partialEncode : Encoder a -> a -> (Hexstring, Hexstring) -> (Hexstring, Hexstring)
partialEncode encoder data hexstr_pair = encoder data hexstr_pair

uint256 : Encoder UInt256
uint256 integer =
            integer
                |> uint256ToBigInt
                |> unsafeBigInt



int256 : Encoder Int256
int256 integer hexstr_pair =
    let
        bigint = int256ToBigInt integer
        twosComplementPow =
            BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 256)
        twos_complement =
            if
                BigInt.gte bigint (BigInt.fromInt 0) then
                bigint
            else
                BigInt.add twosComplementPow bigint
    in
        unsafeBigInt bigint hexstr_pair


bool : Encoder Bool
bool boolean =
    if boolean then
        unsafeBigInt (BigInt.fromInt 1)
    else
        unsafeBigInt (BigInt.fromInt 0)

bytes32 : Encoder Bytes32
bytes32 bytes (hexstr_head, hexstr_tail) =
    let
        head =
            bytes
                |> bytesToHex
                |> padRightTo32Bytes '0'
    in
        (hexstr_head ++ head, hexstr_tail)

-- Helper:

{-| Unsafe!
Only call after making sure that BigInt is expressible in Int256 resp Uint256!
(This function exists for the overlap in functionality between int256 and uint256 and bool)
 -}
unsafeBigInt : Encoder BigInt
unsafeBigInt bigint (hexstr_head, hexstr_tail) =
    let
        head =
            bigint
                |> BigInt.toHexString
                |> padLeftTo32Bytes '0'
    in
        (hexstr_head ++ head, hexstr_tail)




padLeftTo32Bytes : Char -> String -> String
padLeftTo32Bytes char str =
    String.padLeft 64 char str


padRightTo32Bytes : Char -> String -> String
padRightTo32Bytes char str =
    String.padRight 64 char str


bytesToHex : Bytes32 -> String
bytesToHex (Bytes32 str) =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""

bytesToStr : String -> Result String String
bytesToStr bytes =
    let
        -- two hexchars -> 0..255 -> char
        byteToChar = (Hex.fromString >> Result.map (Char.fromCode))
    in
    bytes
        |> stringGroupsOf 2
        |> List.map byteToChar
        |> Result.Extra.combine
        |> Result.map String.fromList

trimBytesLeft len str = String.dropLeft (64 - (2 * len)) str
trimBytesRight len str = String.dropRight (64 - (2 * len)) str

stringGroupsOf : Int -> String -> List String
stringGroupsOf num str =
    str
        |> String.toList
        |> List.Extra.groupsOf num
        |> List.map String.fromList
