module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg)
import Routing
import Navigation
import Web3
import Porter

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
        Msgs.NetVersion version_num ->
            let
                _ = Debug.log ("version num:" ++ (toString version_num)) ()
            in
                (model, Cmd.none)
        Msgs.ClientVersion version_str ->
            let
                _ = Debug.log ("client version: " ++ (toString version_str))
            in
                (model, Cmd.none)
        Msgs.NetListening is_listening ->
            let
                _ = Debug.log ("net_listening: " ++ (toString is_listening))
            in
                (model, Cmd.none)
        Msgs.Foo stuff ->
            Debug.log ("Foo:" ++ (toString stuff))
            (model, Cmd.none)


updateLocationChange location model =
            let
                newRoute =
                    Routing.parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )


updateBlockNumber block_number model =
            let
                old_eth_info = model.ethereum_info
                ethereum_info = {old_eth_info | block_depth = toString block_number}
            in
                {model | ethereum_info = ethereum_info} ! []
