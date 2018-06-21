module Routing exposing (..)

import Navigation
import Models exposing (ListingId, Route(..))
import UrlParser exposing ((</>))


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [UrlParser.map ListingsRoute UrlParser.top
        , UrlParser.map ListingRoute (UrlParser.s "listings" </> UrlParser.string)
        , UrlParser.map ListingsRoute (UrlParser.s "listings")
        ]

parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route
        Nothing ->
            NotFoundRoute


listingsPath : String
listingsPath = "#listings"

listingPath : ListingId -> String
listingPath listing_id = "#listings/" ++ listing_id
