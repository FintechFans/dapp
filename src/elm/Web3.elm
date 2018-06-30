port module Web3 exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Porter
import Msgs exposing (Msg)
import Web3.Types exposing (Web3RPCCall, Web3RPCResponse(..), Request, Config, Error, Address, UnformattedData, Sha3Hash)
import Web3.Utils
import BigInt exposing (BigInt)
import Result.Extra
import Json.Decode.Extra as DecodeExtra
import Web3.Decode


port outgoing : Encode.Value -> Cmd msg


port incoming : (Decode.Value -> msg) -> Sub msg


porterConfig : Config Msg
porterConfig =
    { outgoingPort = outgoing
    , incomingPort = incoming
    , encodeRequest = web3_call_encoder
    , decodeResponse = web3_call_decoder
    , porterMsg = Msgs.Web3Msg
    }


{-| Encodes a Web3RPCCall object into a JSON-RPC-ready JavaScript object
-}
web3_call_encoder : Web3RPCCall -> Encode.Value
web3_call_encoder web3_rpc_call =
    Encode.object
        [ ( "jsonrpc", Encode.string "2.0" )
        , ( "method", Encode.string web3_rpc_call.method )
        , ( "params", web3_rpc_call.params |> List.map Encode.string |> Encode.list )
        ]


{-| Decodes a response from a JavaScript-supplied JSON-RPC-response into a Web3RPCResponse value
-}
web3_call_decoder : Decode.Decoder Web3RPCResponse
web3_call_decoder =
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


{-| Should be added to your application so Web3 is able to listen to incoming responses from the JS web3 library.
-}
subscriptions : Sub Msg
subscriptions =
    Porter.subscriptions porterConfig


{-| Should be added to your application so Web3 is able to chain requests/responses made using its library.
-}
update porter_msg model =
    let
        ( porter_model, porter_cmd ) =
            Porter.update porterConfig porter_msg model.web3_porter
    in
        -- Debug.log (toString porter_msg)
        ( { model | web3_porter = porter_model }, porter_cmd )


{-| Low-level function that manually creates a request.
Can be used to send a request that Web3 does not currently support itself,
but this is not advised, since the chance of errors is high
(resulting in always `Web3.Types.Error`-responses being returned)
so only do this as a last resort.

Converting parameters to hex-format and converting the responses from hex format to their actual types has to be done manually as well.

Decoding happens as follows:

1.  `web3_call_decoder` transforms the incoming JSON to a `Web3RPCResponse`
2.  `interpretJSONRPCResult` transforms the `Web3RPCResponse` into a `Result Error Decode.Value`
3.  The specified `request_decoder` transforms this `Decode.Value` into `res`.

-}
request : Web3RPCCall -> Decode.Decoder res -> Request res
request call_info request_decoder =
    Web3.Types.Request (Porter.request call_info) (interpretJSONRPCResult >> Result.andThen (Web3.Decode.decodeValue request_decoder))


{-| Performs a (chain of) request(s) constructed using this library

The given `msgHandler` will be called with the resulting answer.

-}
send : Config msg -> (Result Error res -> msg) -> Request res -> Cmd msg
send config msg_handler (Web3.Types.Request porter_request result_handler) =
    Porter.send config (result_handler >> msg_handler) porter_request


{-| Internal function that decodes the result of a JSON-RPC call,
mapping JSON-RPC error codes to their proper Web3.Types.Error instance,
and passing a successful response to the supplied `decoder`.
-}
interpretJSONRPCResult : Web3RPCResponse -> Result Error Decode.Value
interpretJSONRPCResult response =
    case response of
        SuccessfulResponse val ->
            Result.Ok val

        ErrorResponse -32700 str ->
            Result.Err (Web3.Types.ServerError (Web3.Types.ParseError str))

        ErrorResponse -32600 str ->
            Result.Err (Web3.Types.ServerError (Web3.Types.InvalidRequest str))

        ErrorResponse -32601 str ->
            Result.Err (Web3.Types.ServerError (Web3.Types.MethodNotFound str))

        ErrorResponse -32602 str ->
            Result.Err (Web3.Types.ServerError (Web3.Types.InvalidParams str))

        ErrorResponse -32603 str ->
            Result.Err (Web3.Types.ServerError (Web3.Types.InternalError str))

        ErrorResponse code str ->
            Result.Err (Web3.Types.ServerError (Web3.Types.UnknownError code str))


clientVersion : Request String
clientVersion =
    request { method = "web3_clientVersion", params = [] } Decode.string


netVersion : Request String
netVersion =
    request { method = "net_version", params = [] } Decode.string


netListening : Request Bool
netListening =
    request { method = "net_listening", params = [] } Decode.bool


netPeerCount : Request BigInt
netPeerCount =
    request { method = "net_peerCount", params = [] } Web3.Decode.big_int


ethGasPrice : Request BigInt
ethGasPrice =
    request { method = "eth_gasPrice", params = [] } Web3.Decode.big_int


web3Sha3 : String -> Request Sha3Hash
web3Sha3 str =
    let
        hexstring =
            str |> Web3.Utils.unformattedDataToHexString
    in
        request { method = "web3_sha3", params = [ hexstring ] } Web3.Decode.unformatted_data


ethAccounts : Request (List Address)
ethAccounts =
    request { method = "eth_accounts", params = [] } (Decode.list Web3.Decode.address)
