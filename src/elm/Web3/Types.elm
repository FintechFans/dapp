module Web3.Types exposing (..)

import Porter
import Json.Decode as Decode
import Json.Encode as Encode



type alias Model msg_type = Porter.Model Web3RPCCall Web3RPCResponse msg_type

type alias Web3RPCCall =
    { method : String
    , params: List String
    }

type alias Web3RPCResponse =
    { result: Decode.Value
    }

type alias Request res = Porter.RequestWithHandler Web3RPCCall Web3RPCResponse (Result String res)

type alias Message msg = Porter.Msg Web3RPCCall Web3RPCResponse msg

type alias Config msg = Porter.Config Web3RPCCall Web3RPCResponse msg

init = Porter.init

{-| TODO Fill in known networks here -}
type alias NetworkVersion = Int

{-| A type representing the kind of software your client is running
 -}
type alias ClientVersion = String

{-| Used for the following requests:

  - eth_getBalance
  - eth_getCode
  - eth_getTransactionCount
  - eth_getStorageAt
  - eth_call

See https://github.com/ethereum/wiki/wiki/JSON-RPC#the-default-block-parameter
 -}
type BlockParameter = Hex Int | Earliest | Latest | Pending
