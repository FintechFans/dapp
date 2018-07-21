module EthAbi.Decoder exposing (..)

import Char
import Hex
import List.Extra
import Result.Extra

-- TODO proper int256 type that is large enough (BigInt wrapper)
type alias Int256 = Int
-- TODO proper uint256 type that is large enough (BigInt wrapper)
type alias UInt256 = Int

-- TODO two's complement
int256 : String -> Result String Int256
int256 hexstr =
    hexstr
        |> ensureSingleWord
        |> Result.andThen Hex.fromString

uint256 : String -> Result String UInt256
uint256 hexstr =
    hexstr
        |> ensureSingleWord
        |> Result.andThen Hex.fromString

bool : String -> Result String Bool
bool hexstr =
    let
        intToBool num = case num of
                         0 -> Ok False
                         1 -> Ok True
                         _ -> Err "Impossible to convert ABI-encoded value to boolean, not '0' or '1'"
    in
    hexstr
        |> ensureSingleWord
        |> Result.andThen Hex.fromString
        |> Result.andThen intToBool

static_bytes len hexstr =
    hexstr
        |> ensureSingleWord
        |> Result.map (trimBytesRight len)
        |> Result.andThen bytesToStr

trimBytesLeft len str = String.dropLeft (64 - (2 * len)) str
trimBytesRight len str = String.dropRight (64 - (2 * len)) str


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

stringGroupsOf : Int -> String -> List String
stringGroupsOf num str =
    str
        |> String.toList
        |> List.Extra.groupsOf num
        |> List.map String.fromList

ensureSingleWord hexstr =
    if String.length hexstr == 64 then Ok hexstr else Err "Given ABI hexadecimal string is not 256 bits"