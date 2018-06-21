module View exposing (..)

import Html exposing (Html, div, text)
import Msgs exposing (Msg)
import Models exposing (Model)
import Listings
import Dict

view : Model -> Html Msg
view model =
    div []
        [ page model ]

page : Model -> Html Msg
page model =
    case model.route of
        Models.ListingsRoute ->
            model.listings
                |> Dict.values
                |> Listings.view
        Models.ListingRoute listing_id ->
            let
                maybePlayer =
                    Dict.get listing_id model.listings
            in
                case maybePlayer of
                    Nothing ->
                        notFoundView
                    Just listing ->
                        Listings.listing_view listing
        Models.NotFoundRoute ->
            notFoundView

notFoundView : Html msg
notFoundView =
    div []
        [text "Error: Page not found. Please check the URL and try again"
        ]

