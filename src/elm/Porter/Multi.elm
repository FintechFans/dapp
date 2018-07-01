module Porter.Multi exposing (..)

import Porter


type Request req res a
    = SimpleRequest (Porter.Request req res) (res -> a)
    | ComplexRequest (Porter.Request req res) (res -> Request req res a)


type alias Config req res msg a =
    { porter_config : Porter.Config req res msg
    , porter_multi_msg : Msg req res msg a -> msg
    }


type Msg req res msg a
    = PorterMsg (Porter.Msg req res msg)
    | ResolveChain (a -> msg) res (Request req res a)


type alias Model req res msg =
    { porter_model : Porter.Model req res msg }


init : Model req res msg
init =
    { porter_model = Porter.init }


request : req -> (res -> a) -> Request req res a
request req response_handler =
    SimpleRequest (Porter.request req) response_handler


andThen : (a -> Request req res b) -> Request req res a -> Request req res b
andThen reqfun req =
    case req of
        SimpleRequest porter_req request_mapper ->
            ComplexRequest (porter_req) (request_mapper >> reqfun)

        ComplexRequest porter_req next_request_fun ->
            ComplexRequest porter_req (\res -> andThen reqfun (next_request_fun res))


send : Config req res msg a -> (a -> msg) -> Request req res a -> Cmd msg
send config msg_handler request =
    case request of
        SimpleRequest porter_req response_handler ->
            Porter.send (config.porter_config) (response_handler >> msg_handler) porter_req

        ComplexRequest porter_req next_request_fun ->
            Porter.send (config.porter_config) (\res -> config.porter_multi_msg (ResolveChain msg_handler res (next_request_fun res))) porter_req


update : Config req res msg a -> Msg req res msg a -> Model req res msg -> ( Model req res msg, Cmd msg )
update config msg model =
    case msg of
        PorterMsg porter_msg ->
            let
                ( porter_model, porter_cmd ) =
                    Porter.update config.porter_config porter_msg model.porter_model
            in
                ( { model | porter_model = porter_model }, porter_cmd )

        ResolveChain msg_handler res request ->
            ( model, send config msg_handler request )
