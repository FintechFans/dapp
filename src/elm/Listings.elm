module Listings exposing (view, listing_view)

import Html exposing (Html, div, text, ul, li, a, i)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Msgs exposing (Msg)
import Models exposing (Model, ListingId, Listing)
import Routing
import Common.ViewHelpers exposing (linkTo)

view : List Listing -> Html Msg
view listings =
    div [class "ui feed"]
        (List.map listingElem listings)

listingElem : Listing -> Html Msg
listingElem listing =
    let
        path = Routing.listingPath listing.id
    in
        linkTo path [class "event"]
            [ div [class "content"]
                  [ div [class "summary"]
                        [ text listing.title
                        , div [class "date"] [text "1 day ago"]
                        ]
                  , div [class "meta"]
                      [rating_view listing.rating
                      ]
                  ]
            ]

listing_view : Listing -> Html Msg
listing_view listing=
    div [] [text ("TEST " ++ (listing.id))]

rating_view : Int -> Html Msg
rating_view rat =
    div [class "ui star rating"]
        [ i [class "ui icon active"] []
        , i [class "ui icon active"] []
        , i [class "ui icon active"] []
        , i [class "ui icon "] []
        , i [class "ui icon "] []
        ]
