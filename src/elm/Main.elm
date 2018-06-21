
module Main exposing (..)

import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Navigation
import Routing
import Update
import View
import Eth
import Task


init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let currentRoute =
            Routing.parseLocation location
        model = initialModel currentRoute
    in
        model !
            [ (Eth.getBlockNumber model.ethereum_info.provider)
                  |> Task.onError (\err -> Debug.log (toString err) (Task.succeed 0))
                  |> Task.perform Msgs.EthBlockNumberKnown
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    Navigation.program Msgs.LocationChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
