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
import Porter


init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let currentRoute =
            Routing.parseLocation location
        model = initialModel currentRoute
        eth_task =
            (Eth.getBlockNumber model.ethereum_info.provider)
                |> Task.onError (\err -> Debug.log (toString err) (Task.succeed 0))
                |> Task.perform Msgs.EthBlockNumberKnown
        porter_send_text =
            Porter.send Msgs.Foo "Some Text" |> Cmd.map Msgs.Web3Msg
    in
        model !
            [ eth_task
            , porter_send_text
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Web3.porter_subscriptions

main : Program Never Model Msg
main =
    Navigation.program Msgs.LocationChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
