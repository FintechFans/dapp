module EthAbi.Decoder exposing (int256, uint256, bool, static_bytes)

import Char
import Hex
import List.Extra
import Result.Extra
import BigInt
import EthAbi.Types exposing (Int256, UInt256, Bytes32)


type alias EthAbiDecoder t =
    String -> Result String t

succeed : a -> EthAbiDecoder a
succeed val = \_ -> Ok val

fail : String -> EthAbiDecoder a
fail error_message = \_ -> Err error_message

map : (a -> b) -> EthAbiDecoder a -> EthAbiDecoder b
map fun decoder =
    decoder >> Result.map fun


map2 : (a -> b -> c) -> EthAbiDecoder a -> EthAbiDecoder b -> EthAbiDecoder b
map2 fun decodera decoderb =
    Result.map2 decodera decoderb fun

decode : a -> EthAbiDecoder a
decode =
    succeed



-- TODO two's complement


int256 : EthAbiDecoder Int256
int256 hexstr =
    hexstr
        |> ensureSingleWord
        |> Result.andThen hexToBigInt
        |> Result.andThen EthAbi.Types.int256


uint256 : EthAbiDecoder UInt256
uint256 hexstr =
    hexstr
        |> ensureSingleWord
        |> Result.andThen hexToBigInt
        |> Result.andThen EthAbi.Types.uint256


bool : EthAbiDecoder Bool
bool hexstr =
    let
        intToBool num =
            case num of
                0 ->
                    Ok False

                1 ->
                    Ok True

                _ ->
                    Err "Impossible to convert ABI-encoded value to boolean, not '0' or '1'"
    in
        hexstr
            |> ensureSingleWord
            |> Result.andThen Hex.fromString
            |> Result.andThen intToBool


static_bytes : Int -> EthAbiDecoder Bytes32
static_bytes len hexstr =
    hexstr
        |> ensureSingleWord
        |> Result.map (trimBytesRight len)
        |> Result.andThen bytesToStr
        |> Result.andThen (EthAbi.Types.bytes len)


trimBytesLeft len str =
    String.dropLeft (64 - (2 * len)) str


trimBytesRight len str =
    String.dropRight (64 - (2 * len)) str


hexToBigInt hex =
    BigInt.fromString ("0x" ++ hex) |> Result.fromMaybe "Could not parse hex as BigInt"


bytesToStr : String -> Result String String
bytesToStr bytes =
    let
        -- two hexchars -> 0..255 -> char
        byteToChar =
            (Hex.fromString >> Result.map (Char.fromCode))
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
    if String.length hexstr == 64 then
        Ok hexstr
    else
        Err "Given ABI hexadecimal string is not 256 bits"

tupleTODO = Decode.map2
