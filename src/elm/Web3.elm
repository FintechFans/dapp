port module Web3 exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Porter
import Msgs exposing (Msg)

port outgoing : Encode.Value -> Cmd msg
port incoming : (Decode.Value -> msg) -> Sub msg

porterConfig : Porter.Config String String Msg
porterConfig =
    {outgoingPort = outgoing
    , incomingPort = incoming
    , encodeRequest = Encode.string
    , decodeResponse = Decode.string
    }

porter_subscriptions : Sub Msg
porter_subscriptions =
    Porter.subscriptions porterConfig |> Sub.map Msgs.Web3Msg


porter_update porter_msg model =
            let
                (porter_model, porter_cmd) =
                    Porter.update porterConfig porter_msg model.web3_porter
            in
                Debug.log (toString porter_msg)
                    <| ({ model | web3_porter = porter_model }, porter_cmd)
