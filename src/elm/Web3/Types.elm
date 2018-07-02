module Web3.Types exposing (..)

import Porter.Multi
import Json.Decode as Decode
import Json.Encode as Encode
import BigInt exposing (BigInt)


type alias Model msg_type =
    Porter.Multi.Model Web3RPCCall Web3RPCResponse msg_type


type alias Web3RPCCall =
    { method : String
    , params : List Encode.Value
    }


type Web3RPCResponse
    = SuccessfulResponse Decode.Value
    | ErrorResponse Int String


type alias Request res
    = Porter.Multi.Request Web3RPCCall Web3RPCResponse (Result Error res)
    -- = Request (Porter.Request Web3RPCCall Web3RPCResponse) (Web3RPCResponse -> Result Error res)


type alias Message msg =
    Porter.Multi.Msg Web3RPCCall Web3RPCResponse msg


type alias Config msg =
    Porter.Multi.Config Web3RPCCall Web3RPCResponse msg


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
    Porter.Multi.init


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
    = BlockNumber BigInt
    | Earliest
    | Latest
    | Pending



-- TODO Hide constructor


type Address
    = Address String


type alias UnformattedData =
    String


type alias Sha3Hash =
    String


type NetworkVersion
    = Mainnet
    | Testnet TestnetVersion
    | UnknownNetwork String


type TestnetVersion
    = Morden
    | Ropsten
    | Rinkeby
    | Kovan


type Syncing
    = NotSyncing
    | Syncing { startingBlock : BigInt, currentBlock : BigInt, highestBlock : BigInt }

type alias BlockInfo =
    { number : Maybe BigInt
    , hash : Maybe Sha3Hash
    , parentHash : Sha3Hash
    , nonce : Maybe String
    , sha3Uncles : Sha3Hash
    , logsBloom : String
    , transactionsRoot : String
    , stateRoot : String
    , receiptsRoot : String
    , miner : Address
    , difficulty : BigInt
    , totalDifficulty : BigInt
    , extraData : String
    , size : BigInt
    , gasLimit : BigInt
    , gasUsed : BigInt
    , timestamp : BigInt
    , transactions : List (Decode.Value)
    , uncles : List (Decode.Value)
    }
