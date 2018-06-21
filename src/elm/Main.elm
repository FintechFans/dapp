-- import Html exposing (Html, text, button, div, h1)
-- import Html.Events exposing (onClick)
-- import Random
-- import List.Nonempty

-- main : Program Never Model Msg
-- main = Html.program
--        { init = init
--        , view = view
--        , update = update
--        , subscriptions = subscriptions
--        }

-- type alias Model =
--     { oracleAnswer : String
--     }

-- type Msg = Foo
--     | Roll
--     | NewFace Int

-- choices : List.Nonempty.Nonempty String
-- choices = List.Nonempty.Nonempty "Mango"
--           [ "Tropical Fruits"
--           , "Melon"
--           ]


-- subscriptions : Model -> Sub Msg
-- subscriptions model = Sub.none

-- init : (Model, Cmd Msg)
-- init = (Model "???", Cmd.none)

-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--     case msg of
--         Roll ->
--             (model, Random.generate NewFace (Random.int 0 (List.Nonempty.length choices)))
--         NewFace newFace ->
--             (Model (List.Nonempty.get newFace choices), Cmd.none)
--         _ ->
--             (model, Cmd.none)


-- view : Model -> Html Msg
-- view model = div []
--              [ h1 [] [text (model.oracleAnswer)]
--              , button [ onClick Roll ] [ text "Consult the Oracle"]
--              ]

module Main exposing (..)

import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Navigation
import Routing
import Update
import View


init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let currentRoute =
            Routing.parseLocation location
    in
        ( initialModel currentRoute, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main =
    Navigation.program Msgs.LocationChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
