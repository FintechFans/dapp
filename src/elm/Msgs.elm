module Msgs exposing (..)

import Navigation exposing (Location)
-- type alias Web3Message = Porter.Msg String String Msg
import BigInt exposing (BigInt)
import Web3.Types

type Msg
    = NavigateTo String
    | LocationChange Location
    | EthBlockNumberKnown (Result Web3.Types.Error BigInt)
    | Web3Msg (Web3.Types.Message Msg) -- Internal Msg used by Web3
    | PrintDebug String String
