module Web3.Types exposing (..)

import Porter
import Json.Decode



type alias Model msg_type = Porter.Model Web3RPCCall Web3RPCResponse msg_type

type alias Web3RPCCall =
    { method : String
    , params: List String
    }

type alias Web3RPCResponse =
    { result: Json.Decode.Value
    }


type alias Message msg_type = Porter.Msg Web3RPCCall Web3RPCResponse msg_type

init = Porter.init

type alias NetworkVersion = Int
