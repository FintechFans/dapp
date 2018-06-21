module Listings exposing (view, listing_view)

import Html exposing (Html, div, text, ul, li, a, i, span)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Msgs exposing (Msg)
import Models exposing (Model, ListingId, Listing, PageContent, Route(..))
import Routing
import Common.ViewHelpers exposing (linkTo)

view : List Listing -> PageContent Msg
view listings =
    let
        title = text "Listings"
        content =
          div [class "ui feed"]
              (List.map listingElem listings)
    in
        { content = content
        , breadcrumbs = listingsBreadcrumbs
        , title = title
        }
listingsBreadcrumbs = [(ListingsRoute, "Listings")]

listingElem : Listing -> Html Msg
listingElem listing =
    let
        path = Routing.listingPath listing.id
    in
        linkTo path [class "event listing-item"]
            [ div [class "content"]
                  [ div [class "summary"]
                        [ span [class "ui green label"] [text "For Hire"]
                        , span [] [text listing.title]
                        , div [class "date"] [text "1 day ago"]
                        ]
                  , div [class "meta"]
                      [rating_view listing.rating
                      ]
                  ]
            ]

listing_view : Listing -> PageContent Msg
listing_view listing =
        { title = text ("Listing " ++ listing.id)
        , breadcrumbs = listingsBreadcrumbs ++ [(ListingRoute listing.id, listing.title)]
        , content = div [] [text ("TEST " ++ (listing.id))]
        }

rating_view : Int -> Html Msg
rating_view rat =
    div [class "ui star rating"]
        [ i [class "ui icon active"] []
        , i [class "ui icon active"] []
        , i [class "ui icon active"] []
        , i [class "ui icon "] []
        , i [class "ui icon "] []
        ]
