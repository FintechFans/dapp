module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg)
import Routing
import Navigation

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.NavigateTo path ->
            (model, Navigation.newUrl <| path)
        Msgs.LocationChange location ->
            updateLocationChange location model
        Msgs.EthBlockNumberKnown block_number ->
            updateBlockNumber block_number model
        other ->
            Debug.log (toString other) <|
            ( model, Cmd.none )

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
