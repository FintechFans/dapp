module Msgs exposing (..)

import Navigation exposing (Location)
import Porter
-- type alias Web3Message = Porter.Msg String String Msg
import BigInt exposing (BigInt)
import Web3.Types

type Msg
    = Foo String
    | NetVersion String
    | NetPeerCount BigInt
    | ClientVersion String
    | NetListening Bool
    | NavigateTo String
    | LocationChange Location
    | EthBlockNumberKnown Int
    | Web3Msg (Web3.Types.Message Msg)
