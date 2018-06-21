module Listings exposing (view, listing_view)

import Html exposing (Html, div, text, ul, li, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Msgs exposing (Msg)
import Models exposing (Model, ListingId, Listing)
import Routing
import Common.ViewHelpers exposing (linkTo)

view : List Listing -> Html Msg
view listings =
    ul []
        (List.map listingElem listings)

listingElem : Listing -> Html Msg
listingElem listing =
    let
        path = Routing.listingPath listing.id
    in
        li []
            [ linkTo path [] [text listing.title]
            ]

listing_view : Listing -> Html Msg
listing_view listing=
    div [] [text ("TEST " ++ (listing.id))]
