port module Main exposing (..)

import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Navigation
import Routing
import Update
import View
import Web3
import Web3.Types
import Result.Extra


-- import Porter

import BigInt


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location

        model =
            initialModel currentRoute

        bigint_debug_conversion =
            (Result.map BigInt.toString >> toString)

        debug_send conversion name =
            Web3.send web3_config (conversion >> Msgs.PrintDebug name)
        eth_blocknum_task =
            Web3.ethBlockNumber
                |> Web3.send web3_config (Result.Extra.unpack (toString >> Msgs.PrintDebug "Error While fetching ethBlockNumber") Msgs.EthBlockNumberKnown)

        complex_task =
            Web3.ethBlockNumber
                |> Web3.andThen (\res -> Web3.ethGetBlockByNumber res True)
                |> debug_send toString "complex_task; Fetching information of latest block."
    in
        model
            ! [ eth_blocknum_task
              , Web3.netVersion |> debug_send toString "netVersion"
              , Web3.ethSyncing |> debug_send toString "ethSyncing"
              , Web3.ethCoinbase |> debug_send toString "ethCoinbase"
              , Web3.netListening |> debug_send toString "netListening"
              , Web3.ethMining |> debug_send toString "ethMining"
              , Web3.clientVersion |> debug_send toString "clientVersion"
              , Web3.ethProtocolVersion |> debug_send toString "ethProtocolVersion"
              , Web3.ethAccounts |> debug_send toString "ethAccounts"
              , Web3.ethGasPrice |> debug_send bigint_debug_conversion "ethGasPrice"
              , Web3.netPeerCount |> debug_send bigint_debug_conversion "netPeerCount"
              , Web3.ethBlockNumber |> debug_send bigint_debug_conversion "ethBlockNumber"
              , Web3.web3Sha3 "hello world" |> debug_send toString "web3Sha3 of 'hello world'"
              , complex_task
              ]



-- testCalls =
--     let
--         str_calls =
--             [ ( Web3.clientVersion, "clientVersion" )
--             , ( Web3.ethProtocolVersion, "ethProtocolVersion" )
--             ]
--         bool_calls =
--             [ ( Web3.netVersion, "netVersion" )
--             , ( Web3.ethSyncing, "ethSyncing" )
--             , ( Web3.netListening, "netListening" )
--             , ( Web3.ethMining, "ethMining" )
--             ]
--             -- , ( Web3.ethCoinbase, "ethCoinbase" )
--             -- , ( Web3.ethGasPrice, "ethGasPrice" )
--             -- , ( Web3.netPeerCount, "netPeerCount" )
--             -- , ( Web3.ethAccounts, "ethAccounts" )
--     in
--         str_calls


subscriptions : Model -> Sub Msg
subscriptions model =
    Web3.subscriptions web3_config


main : Program Never Model Msg
main =
    Navigation.program Msgs.LocationChange
        { init = init
        , view = View.view
        , update = Update.update web3_config
        , subscriptions = subscriptions
        }


web3_config : Web3.Types.Config Msg
web3_config =
    Web3.config Msgs.Web3Msg
