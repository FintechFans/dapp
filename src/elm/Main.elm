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
            -- Web3.send Msgs.Foo "Some text"
            -- Porter.sendEffectful (Cmd.map Msgs.Foo) "Some text"
            --     |> Task.perform Cmd.map Msgs.Web3Msg
            -- Porter.sendEffectful do_something "Some Text"
            -- |> Task.andThen(\res -> Porter.sendEffectful (Cmd.map Msgs.Foo) ("Some Text2" ++ res))
            --     |> Cmd.map Msgs.Web3Msg
            Web3.netVersion Msgs.NetVersion
    in
        model !
            [ eth_task
            , porter_send_text
            ]

do_something res =
    res
        -- |> Task.succeed
        -- |> Task.andThen (Porter.sendEffectful do_something2 ("Some More Text" ++ res))
        -- |> Task.perform (Cmd.map Msgs.Web3Msg)
    -- Task.perform Msgs.Foo (Task.succeed res)
    -- Msgs.Foo res

do_something2 res =
    Msgs.Foo res

subscriptions : Model -> Sub Msg
subscriptions model =
    Web3.subscriptions

main : Program Never Model Msg
main =
    Navigation.program Msgs.LocationChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
