module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg)
import Routing
import Navigation
import Web3
import Porter
import BigInt

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.NavigateTo path ->
            (model, Navigation.newUrl <| path)
        Msgs.LocationChange location ->
            updateLocationChange location model
        Msgs.EthBlockNumberKnown block_number ->
            updateBlockNumber block_number model
        Msgs.Web3Msg web3_msg ->
            Web3.update web3_msg model
        Msgs.PrintDebug name content ->
            let
                _ = Debug.log ("Debugging: " ++ name ++ " ===> `" ++ content ++ "`") ()
            in
                (model, Cmd.none)


updateLocationChange : Navigation.Location -> Model -> (Model, Cmd Msg)
updateLocationChange location model =
            let
                newRoute =
                    Routing.parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

updateBlockNumber : Int -> Model -> (Model, Cmd Msg)
updateBlockNumber block_number model =
            let
                old_eth_info = model.ethereum_info
                ethereum_info = {old_eth_info | block_depth = toString block_number}
            in
                {model | ethereum_info = ethereum_info} ! []
