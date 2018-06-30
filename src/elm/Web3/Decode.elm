module Web3.Decode exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Result
import BigInt exposing (BigInt)
import Web3.Types exposing (UnformattedData, Error(..), Address)
import Web3.Utils


decodeValue : Decode.Decoder a -> Decode.Value -> Result Error a
decodeValue decoder res =
    res
        |> Decode.decodeValue decoder
        |> Result.mapError Web3.Types.ResultParseError


big_int : Decode.Decoder BigInt
big_int =
    Decode.string
        |> Decode.andThen (DecodeExtra.fromResult << Web3.Utils.hexQuantityToBigInt)


unformatted_data : Decode.Decoder UnformattedData
unformatted_data =
    Decode.string
        |> Decode.andThen (DecodeExtra.fromResult << Web3.Utils.hexStringToUnformattedData)


address : Decode.Decoder Address
address =
    Decode.string
        |> Decode.andThen (DecodeExtra.fromResult << Web3.Utils.hexStringToAddress)
