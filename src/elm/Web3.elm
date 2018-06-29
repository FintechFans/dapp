port module Web3 exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Porter
import Msgs exposing (Msg)
import Web3.Types exposing (Web3RPCCall, Web3RPCResponse, Request, Config)

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
        [ ("jsonrpc", Encode.string "2.0")
        , ("method", Encode.string web3_rpc_call.method)
        , ("params", web3_rpc_call.params |> List.map Encode.string |> Encode.list)
        ]

web3_call_decode =
    Decode.field "result" Decode.value
        |> Decode.map Web3RPCResponse


subscriptions : Sub Msg
subscriptions =
    Porter.subscriptions porterConfig


update porter_msg model =
            let
                (porter_model, porter_cmd) =
                    Porter.update porterConfig porter_msg model.web3_porter
            in
                -- Debug.log (toString porter_msg)
                    ({ model | web3_porter = porter_model }, porter_cmd)

request : Web3RPCCall -> (Web3RPCResponse -> (Result String res)) -> Request res
request call_info request_handler =
    Porter.fancyRequest call_info request_handler

decodeResult : (Decode.Decoder res) -> Web3RPCResponse -> Result String res
decodeResult decoder response =
    response
        |> .result
        |> Decode.decodeValue decoder
        |> Debug.log "decodeResult!"

clientVersion : Request String
clientVersion =
    request {method = "web3_clientVersion", params = []} (decodeResult Decode.string)

netVersion : Request String
netVersion =
    request {method = "net_version", params = []} (decodeResult Decode.string)

netListening : Request Bool
netListening =
    request {method = "net_listening", params = []} (decodeResult Decode.bool)

send : Config msg -> (Result String res -> msg) -> Request res -> Cmd msg
send config msg_handler req =
    Porter.send config msg_handler req
 
-- send response_handler request =
    -- Porter.send (\res -> res |> Debug.log "TEST" |> response_handler) (Porter.request  request)


-- sendMessage response_handler text =
--     let
--         request =
--             { method = "fake_message"
--             , params = [text]
--             }
--     in
--         send response_handler request


-- netVersion : (String -> Msg) -> Cmd Msg
-- netVersion response_handler =
--     let
--         response_wrapper res =
--             res
--                 |> .result
--                 |> Decode.decodeValue (Decode.string)
--                 |> Result.withDefault "-1"
--                 |> response_handler
--     in
--         send (response_wrapper) {method = "net_version", params = []}

-- netListening : (Bool -> Msg) -> Cmd Msg
-- netListening response_handler =
--     let
--         response_wrapper res =
--             res
--                 |> .result
--                    |> Decode.decodeValue (Decode.bool)
--                    |> Result.withDefault False
--                    |> response_handler
--     in
--         send (response_wrapper) {method = "net_listening", params = []}

-- clientVersion : (String -> Msg) -> Cmd Msg
-- clientVersion response_handler =
--     let
--         response_wrapper res =
--             res
--                 |> .result
--                 |> Decode.decodeValue (Decode.string)
--                 |> Result.withDefault "?"
--                 |> response_handler
--     in
--         send (response_wrapper) {method = "web3_clientVersion", params = []}

