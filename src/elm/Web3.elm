port module Web3 exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Porter
import Msgs exposing (Msg)
import Web3.Types exposing (Web3RPCCall)

port outgoing : Encode.Value -> Cmd msg
port incoming : (Decode.Value -> msg) -> Sub msg


porterConfig : Porter.Config Web3RPCCall String Msg
porterConfig =
    { outgoingPort = outgoing
    , incomingPort = incoming
    , encodeRequest = web3_call_encode
    , decodeResponse = Decode.string
    }


web3_call_encode : Web3RPCCall -> Encode.Value
web3_call_encode web3_rpc_call =
    Encode.object
        [ ("jsonrpc", Encode.string "2.0")
        , ("method", Encode.string web3_rpc_call.method)
        , ("params", web3_rpc_call.params |> List.map Encode.string |> Encode.list)
        ]


subscriptions : Sub Msg
subscriptions =
    Porter.subscriptions porterConfig |> Sub.map Msgs.Web3Msg


update porter_msg model =
            let
                (porter_model, porter_cmd) =
                    Porter.update porterConfig porter_msg model.web3_porter
            in
                Debug.log (toString porter_msg)
                    <| ({ model | web3_porter = porter_model }, porter_cmd)

send response_handler request =
    Porter.send (\res -> res |> Debug.log "TEST" |> response_handler) request
        |> Cmd.map Msgs.Web3Msg


sendMessage response_handler text =
    let
        request =
            { method = "fake_message"
            , params = [text]
            }
    in
        send response_handler request


netVersion : (Int -> Msg) -> Cmd Msg
netVersion response_handler =
    let
        response_wrapper res =
            res
                |> Debug.log "RESPONSE WRAPPER"
                |> Decode.decodeString (Decode.int)
                |> Result.withDefault -1
                |> response_handler
    in
        send (response_wrapper) {method = "net_version", params = []}