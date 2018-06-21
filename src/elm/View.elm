module View exposing (..)

import Html exposing (Html, div, text, header, footer)
import Html.Attributes exposing (class)
import Msgs exposing (Msg)
import Models exposing (Model)
import Listings
import Dict
import Common.ViewHelpers exposing (linkTo)

view : Model -> Html Msg
view model =
    div []
        [ header [] [ renderHeader ]
        , div [class "content"] [ pageWrapper model ]
        , footer [] [ renderFooter ]
        ]


pageWrapper : Model -> Html Msg
pageWrapper model =
    div [class "ui container"]
        [ page model ]

renderHeader : Html Msg
renderHeader = div []
         [ renderMenu
         ]

renderMenu : Html Msg
renderMenu = div [class "ui menu"]
             [ linkTo "/" [class "header item"] [text "FintechFans Decentralized Marketplace"]
             , linkTo "/" [class "item"] [text "Home"]
             ]

renderFooter : Html Msg
renderFooter = div [][]


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

