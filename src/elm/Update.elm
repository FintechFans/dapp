module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg)
import Routing

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnLocationChange location ->
            let
                newRoute =
                    Routing.parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
        _ ->
            ( model, Cmd.none )
