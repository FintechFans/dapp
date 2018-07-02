module Web3.Decode exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Result
import BigInt exposing (BigInt)
import Web3.Types exposing (UnformattedData, Error(..), Address, NetworkVersion(..), TestnetVersion(..), Syncing(..))
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

network_version : Decode.Decoder NetworkVersion
network_version =
    let
        version_decoder version =
            case version of
                "1" -> Mainnet
                "2" -> (Testnet Morden)
                "3" -> (Testnet Ropsten)
                "4" -> (Testnet Rinkeby)
                "42" -> (Testnet Kovan)
                _ -> UnknownNetwork version
    in
    Decode.string
        |> Decode.map version_decoder

syncing : Decode.Decoder Syncing
syncing =
    let
        false_decoder =
            Decode.bool
            |> Decode.andThen (\bool ->
                       case bool of
                            True -> Decode.fail "Error: Boolean returned from syncing call cannot be 'true'"
                            False -> Decode.succeed NotSyncing
                         )
        syncing_obj_decoder =
            Decode.map3 (\startingBlock currentBlock highestBlock -> Syncing {startingBlock = startingBlock, currentBlock = currentBlock, highestBlock = highestBlock})
                (Decode.field "startingBlock" big_int)
                (Decode.field "currentBlock" big_int)
                (Decode.field "highestBlock" big_int)
    in
        Decode.oneOf [false_decoder, syncing_obj_decoder]

block_info : Decode.Decode BlockInfo
block_info = 
