module Web3.Decode exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Pipeline as Pipeline
import Result
import BigInt exposing (BigInt)
import Web3.Types exposing (UnformattedData, Error(..), Address, NetworkVersion(..), TestnetVersion(..), Syncing(..), BlockInfo)
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

block_info : Decode.Decoder BlockInfo
block_info =
    Pipeline.decode BlockInfo
        |> Pipeline.required "number" (Decode.nullable big_int)
        |> Pipeline.required "hash" (Decode.nullable Decode.string)
        |> Pipeline.required "parentHash" Decode.string
        |> Pipeline.required "nonce" (Decode.nullable Decode.string)
        |> Pipeline.required "sha3Uncles" Decode.string
        |> Pipeline.required "logsBloom" Decode.string
        |> Pipeline.required "transactionsRoot" Decode.string
        |> Pipeline.required "stateRoot" Decode.string
        |> Pipeline.required "receiptsRoot" Decode.string
        |> Pipeline.required "miner" address
        |> Pipeline.required "difficulty" big_int
        |> Pipeline.required "totalDifficulty" big_int
        |> Pipeline.required "extraData" Decode.string
        |> Pipeline.required "size" big_int
        |> Pipeline.required "gasLimit" big_int
        |> Pipeline.required "gasUsed" big_int
        |> Pipeline.required "timestamp" big_int
        |> Pipeline.required "transactions" (Decode.list Decode.value)
        |> Pipeline.required "uncles" (Decode.list Decode.value)
