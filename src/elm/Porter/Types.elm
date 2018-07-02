module Porter.Types exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Task

{-| Internal type used by requests that have a response handler.
-}
type RequestWithHandler req res msg
    = RequestWithHandler req (List (res -> Request req res)) (res -> msg)


{-| Opaque type of a 'request'. Use the `request` function to create one,
chain them using `andThen` and finally send it using `send`.
-}
type Request req res
    = Request req (List (res -> Request req res))

type MultiRequest req res a
    = SimpleRequest (Request req res) (res -> a)
    | ComplexRequest (Request req res) (res -> MultiRequest req res a)
    | ShortCircuit a

{-| Module messages.
-}
type Msg req res msg
    = SendWithNextId (RequestWithHandler req res msg)
    | Receive Encode.Value
    | ResolveChain (MultiRequest req res msg)

{-| Porter configuration, containing:

  - ports
  - message encoders/decoders.
  - the message that porter will use for its internal communications

-}
type alias Config req res msg =
    { outgoingPort : Encode.Value -> Cmd msg
    , incomingPort : (Encode.Value -> Msg req res msg) -> Sub (Msg req res msg)
    , encodeRequest : req -> Encode.Value
    , decodeResponse : Decode.Decoder res
    , porterMsg : Msg req res msg -> msg
    }


{-| Turns the request's specialized response type into a different type.
-}
internal_multi_map : (a -> b) -> MultiRequest req res a -> MultiRequest req res b
internal_multi_map mapfun req =
    case req of
        SimpleRequest porter_req request_mapper ->
            SimpleRequest porter_req (request_mapper >> mapfun)

        ComplexRequest porter_req next_request_fun ->
            ComplexRequest porter_req (\res -> internal_multi_map mapfun (next_request_fun res))

        ShortCircuit val ->
            ShortCircuit (mapfun val)


{-| Actually sends a (chain of) request(s).

A final handler needs to be specified that turns the final result into a `msg`.
This `msg` will be called with the final resulting `a` once the final response has returned.

-}
internal_multi_send : Config req res msg -> (a -> msg) -> MultiRequest req res a -> Cmd msg
internal_multi_send config msg_handler request =
    let
        mapped_request =
            request |> internal_multi_map msg_handler
    in
        case mapped_request of
            SimpleRequest porter_req response_handler ->
                internal_send (config) response_handler porter_req

            ComplexRequest porter_req next_request_fun ->
                let
                    resfun res =
                        config.porterMsg (ResolveChain (next_request_fun res))
                in
                    internal_send (config) resfun porter_req

            ShortCircuit val ->
                val
                    |> Task.succeed
                    |> Task.perform identity


{-| Sends a request earlier started using `request`.
-}
internal_send: Config req res msg -> (res -> msg) -> Request req res -> Cmd msg
internal_send config responseHandler (Request req reqfuns) =
    internal_runSendRequest config (RequestWithHandler req (List.reverse reqfuns) responseHandler)


{-| Internal function that performs the specified request as a command.
-}
internal_runSendRequest : Config req res msg -> RequestWithHandler req res msg -> Cmd msg
internal_runSendRequest config request =
    SendWithNextId request
        |> Task.succeed
        |> Task.perform identity
        |> Cmd.map config.porterMsg
