port module Main exposing (..)

import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Navigation
import Routing
import Update
import View
import Eth
import Task
import Web3


-- import Porter

import Debug
import BigInt


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location

        model =
            initialModel currentRoute

        eth_task =
            (Eth.getBlockNumber model.ethereum_info.provider)
                |> Task.onError (\err -> Debug.log (toString err) (Task.succeed 0))
                |> Task.perform Msgs.EthBlockNumberKnown

        -- porter_send_text =
        -- Web3.send Msgs.Foo "Some text"
        -- Porter.sendEffectful (Cmd.map Msgs.Foo) "Some text"
        --     |> Task.perform Cmd.map Msgs.Web3Msg
        -- Porter.sendEffectful do_something "Some Text"
        -- |> Task.andThen(\res -> Porter.sendEffectful (Cmd.map Msgs.Foo) ("Some Text2" ++ res))
        --     |> Cmd.map Msgs.Web3Msg
        -- Web3.netListening Msgs.NetListening
    in
        model
            ! [ eth_task
              , Web3.netVersion |> Web3.send web3_config (\res -> res |> Result.withDefault "" |> Msgs.PrintDebug "netVersion")
              , Web3.netListening |> Web3.send web3_config (\res -> res |> Result.withDefault False |> toString |> Msgs.PrintDebug "netListening")
              , Web3.clientVersion |> Web3.send web3_config (\res -> res |> Result.withDefault "" |> Msgs.PrintDebug "clientVersion")
              , Web3.ethGasPrice |> Web3.send web3_config (\res -> res |> Result.withDefault (BigInt.fromInt 0) |> BigInt.toString |> Msgs.PrintDebug "ethGasPrice")
              , Web3.netPeerCount |> Web3.send web3_config (\res -> res |> Result.withDefault (BigInt.fromInt 0) |> BigInt.toString |> Msgs.PrintDebug "netPeerCount")
              , Web3.ethAccounts |> Web3.send web3_config (\res -> res |> Result.withDefault [] |> toString |> Msgs.PrintDebug "ethAccounts")
              ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Web3.subscriptions web3_config


main : Program Never Model Msg
main =
    Navigation.program Msgs.LocationChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }

web3_config = Web3.config Msg.Web3Msg
