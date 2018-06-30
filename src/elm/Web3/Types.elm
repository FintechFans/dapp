module Web3.Types exposing (..)

import Porter
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model msg_type =
    Porter.Model Web3RPCCall Web3RPCResponse msg_type


type alias Web3RPCCall =
    { method : String
    , params : List String
    }


type Web3RPCResponse
    = SuccessfulResponse Decode.Value
    | ErrorResponse Int String


type Request res = Request (Porter.Request Web3RPCCall Web3RPCResponse) (Web3RPCResponse -> Result Error res)


type alias Message msg =
    Porter.Msg Web3RPCCall Web3RPCResponse msg


type alias Config msg =
    Porter.Config Web3RPCCall Web3RPCResponse msg


{-| Errors we might encounter when performing calls:
 - Server (or network)-related errors,
 - Errors related to the parsing of RPC responses.
 -}
type Error
    = ServerError JSONRPCError
    | ResultParseError String

{-| The various types of errors that a JSON-RPC server might return with.
 This library maps the standardized error codes to these readable instances;
 non-standard error codes are mapped to 'UnknownError' which will contain the code in there.
 -}
type JSONRPCError
    = UnknownError Int String
    | ParseError String
    | InvalidRequest String
    | MethodNotFound String
    | InvalidParams String
    | InternalError String


init : Model msg
init =
    Porter.init


{-| TODO Fill in known networks here
-}
type alias NetworkVersion =
    Int


{-| A type representing the kind of software your client is running
-}
type alias ClientVersion =
    String


{-| Used for the following requests:

  - eth_getBalance
  - eth_getCode
  - eth_getTransactionCount
  - eth_getStorageAt
  - eth_call

See <https://github.com/ethereum/wiki/wiki/JSON-RPC#the-default-block-parameter>

-}
type BlockParameter
    = Hex Int
    | Earliest
    | Latest
    | Pending

type Address = Address String
type alias UnformattedData = String
type alias Sha3Hash = String
