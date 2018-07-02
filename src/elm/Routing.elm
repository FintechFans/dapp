module Routing exposing (..)

import Navigation
import Models exposing (ListingId, Route(..))
import UrlParser exposing ((</>))
import Html
import Html.Events
import Json.Decode
import Msgs


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [UrlParser.map ListingsRoute UrlParser.top
        , UrlParser.map ListingRoute (UrlParser.s "listings" </> UrlParser.string)
        , UrlParser.map ListingsRoute (UrlParser.s "listings")
        ]

toPath route =
    case route of
        ListingsRoute ->
            "/"
        ListingRoute listing_id ->
            "/listings/" ++ listing_id
        NotFoundRoute ->
            "/404"

parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parsePath matchers location) of
        Just route ->
            route
        Nothing ->
            NotFoundRoute

linkTo : String -> Html.Attribute Msgs.Msg
linkTo path = Html.Events.onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.Decode.succeed (Msgs.NavigateTo path))

listingsPath : String
listingsPath = "/listings"

listingPath : ListingId -> String
listingPath listing_id = "/listings/" ++ listing_id
