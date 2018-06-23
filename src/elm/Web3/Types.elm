module Web3.Types exposing (..)

import Porter



type alias Model msg_type = Porter.Model Web3RPCCall String msg_type

type alias Web3RPCCall =
    { method : String
    , params: List String
    }


type alias Message msg_type = Porter.Msg Web3RPCCall String msg_type

init = Porter.init


type alias NetworkVersion = Int
