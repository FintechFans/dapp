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
            let
                newRoute =
                    Routing.parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
        _ ->
            ( model, Cmd.none )
