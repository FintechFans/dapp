module Listings exposing (view, listing_view)

import Html exposing (Html, div, text, ul, li, a)
import Html.Attributes exposing (class, href)
import Msgs exposing (Msg)
import Models exposing (Model, ListingId, Listing)
import Routing

view : List Listing -> Html Msg
view listings =
    ul []
        (List.map listingElem listings)

listingElem : Listing -> Html msg
listingElem listing =
    let
        path = Routing.listingPath listing.id
    in
        li []
            [ a [href path] [text listing.title]
            ]

listing_view : Listing -> Html Msg
listing_view listing=
    div [] [text ("TEST " ++ (listing.id))]
