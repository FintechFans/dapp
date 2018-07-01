module Porter.Multi exposing (..)

import Porter
import Json.Encode as Encode
import Json.Decode as Decode


type Request req res a
    = SimpleRequest (Porter.Request req res) (res -> a)
    | ComplexRequest (Porter.Request req res) (res -> Request req res a)


type alias Config req res msg a =
    { outgoingPort : Encode.Value -> Cmd msg
    , incomingPort : (Encode.Value -> Porter.Msg req res msg) -> Sub (Porter.Msg req res msg)
    , encodeRequest : req -> Encode.Value
    , decodeResponse : Decode.Decoder res
    , porterMultiMsg : Msg req res msg a -> msg
    }

configToPorterConfig : Config req res msg a -> Porter.Config req res msg
configToPorterConfig config =
    { porterMsg = (\msg -> (config.porterMultiMsg (PorterMsg msg)))
    , outgoingPort = config.outgoingPort
    , incomingPort = config.incomingPort
    , encodeRequest = config.encodeRequest
    , decodeResponse = config.decodeResponse
    }

type Msg req res msg a
    = PorterMsg (Porter.Msg req res msg)
    | ResolveChain (a -> msg) (Request req res a)


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

map : (a -> b) -> Request req res a -> Request req res b
map mapfun req =
    case req of
        SimpleRequest porter_req request_mapper ->
            SimpleRequest porter_req (request_mapper >> mapfun)
        ComplexRequest porter_req next_request_fun ->
            ComplexRequest porter_req (\res -> map mapfun (next_request_fun res))

map2 : (a -> b -> c) -> Request req res a -> Request req res b -> Request req res c
map2 mapfun req_a req_b =
    req_a
        |> andThen (\res_a -> req_b |> map (mapfun res_a))


map3 : (a -> b -> c -> d) -> Request req res a -> Request req res b -> Request req res c -> Request req res d
map3 mapfun req_a req_b req_c =
    req_a
        |> andThen (\res_a -> map2 (mapfun res_a) req_b req_c)

send : Config req res msg a -> (a -> msg) -> Request req res a -> Cmd msg
send config msg_handler request =
    case request of
        SimpleRequest porter_req response_handler ->
            Porter.send (configToPorterConfig config) (response_handler >> msg_handler) porter_req

        ComplexRequest porter_req next_request_fun ->
            Porter.send (configToPorterConfig config) (\res -> config.porterMultiMsg (ResolveChain msg_handler (next_request_fun res))) porter_req


update : Config req res msg a -> Msg req res msg a -> Model req res msg -> ( Model req res msg, Cmd msg )
update config msg model =
    case msg of
        PorterMsg porter_msg ->
            let
                ( porter_model, porter_cmd ) =
                    Porter.update (configToPorterConfig config) porter_msg model.porter_model
            in
                ( { model | porter_model = porter_model }, porter_cmd )

        ResolveChain msg_handler request ->
            ( model, send config msg_handler request )

subscriptions : Config req res msg a -> Sub msg
subscriptions config =
    Porter.subscriptions (configToPorterConfig config)
