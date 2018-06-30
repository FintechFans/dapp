port module Web3 exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Porter
import Msgs exposing (Msg)
import Web3.Types exposing (Web3RPCCall, Web3RPCResponse(..), Request, Config, Error)
import Web3.Utils
import BigInt exposing (BigInt)


port outgoing : Encode.Value -> Cmd msg


port incoming : (Decode.Value -> msg) -> Sub msg


porterConfig : Config Msg
porterConfig =
    { outgoingPort = outgoing
    , incomingPort = incoming
    , encodeRequest = web3_call_encode
    , decodeResponse = web3_call_decode
    , porterMsg = Msgs.Web3Msg
    }


web3_call_encode : Web3RPCCall -> Encode.Value
web3_call_encode web3_rpc_call =
    Encode.object
        [ ( "jsonrpc", Encode.string "2.0" )
        , ( "method", Encode.string web3_rpc_call.method )
        , ( "params", web3_rpc_call.params |> List.map Encode.string |> Encode.list )
        ]


web3_call_decode : Decode.Decoder Web3RPCResponse
web3_call_decode =
    let
        successful_response_decoder =
            Decode.field "result" Decode.value
                |> Decode.map SuccessfulResponse

        error_response_decoder =
            Decode.field "error" <|
                Decode.map2 ErrorResponse
                    (Decode.field "code" Decode.int)
                    (Decode.field "message" Decode.string)
    in
        Decode.oneOf
            [ successful_response_decoder
            , error_response_decoder
            ]


subscriptions : Sub Msg
subscriptions =
    Porter.subscriptions porterConfig


update porter_msg model =
    let
        ( porter_model, porter_cmd ) =
            Porter.update porterConfig porter_msg model.web3_porter
    in
        -- Debug.log (toString porter_msg)
        ( { model | web3_porter = porter_model }, porter_cmd )


request : Web3RPCCall -> (Web3RPCResponse -> Result Error res) -> Request res
request call_info request_handler =
    Web3.Types.Request (Porter.request call_info) request_handler


send : Config msg -> (Result Error res -> msg) -> Request res -> Cmd msg
send config msg_handler (Web3.Types.Request porter_request result_handler) =
    Porter.send config (result_handler >> msg_handler) porter_request



-- andThen : (Result Error resA -> Web3.Types.Request resB) -> Web3.Types.Request resA -> Web3.Types.Request resB
-- andThen fun (Web3.Types.Request porter_req res_handler) =
--     let
--         foo res = res |> res_handler |> fun
--         bar res =
--             case foo res of
--                 Web3.Types.Request porter_req2 res_handler2 ->
--                     Porter.request porter_req2
--     in
--         Web3.Types.Request (Porter.andThen (bar) porter_req)


decodeJSONRPCResult : Decode.Decoder res -> Web3RPCResponse -> Result Error res
decodeJSONRPCResult decoder response =
    case response of
        SuccessfulResponse val ->
            val
                |> Debug.log "decodeJSONRPCResult Start!"
                |> Decode.decodeValue decoder
                |> Result.mapError Web3.Types.ResultParseError
                |> Debug.log "decodeJSONRPCResult Finish!"

        ErrorResponse -32700 str ->
            Debug.log "decodeJSONRPCResult error case!" <| Result.Err (Web3.Types.ServerParseError str)

        ErrorResponse -32600 str ->
            Debug.log "decodeJSONRPCResult error case!" <| Result.Err (Web3.Types.ServerInvalidRequest str)

        ErrorResponse -32601 str ->
            Debug.log "decodeJSONRPCResult error case!" <| Result.Err (Web3.Types.ServerMethodNotFound str)

        ErrorResponse -32602 str ->
            Debug.log "decodeJSONRPCResult error case!" <| Result.Err (Web3.Types.ServerInvalidParams str)

        ErrorResponse -32603 str ->
            Debug.log "decodeJSONRPCResult error case!" <| Result.Err (Web3.Types.ServerInternalError str)

        ErrorResponse code str ->
            Debug.log "decodeJSONRPCResult error case!" <| Result.Err (Web3.Types.ServerError code str)


clientVersion : Request String
clientVersion =
    request { method = "web3_clientVersion", params = [] } (decodeJSONRPCResult Decode.string)


netVersion : Request String
netVersion =
    request { method = "net_version", params = [] } (decodeJSONRPCResult Decode.string)


netListening : Request Bool
netListening =
    request { method = "net_listening", params = [] } (decodeJSONRPCResult Decode.bool)


netPeerCount : Request BigInt
netPeerCount =
    let
        responseHandler res =
            res
                |> decodeJSONRPCResult Decode.string
                |> Result.andThen Web3.Utils.hexQuantityToBigInt
    in
        request { method = "net_peerCount", params = [] } responseHandler
