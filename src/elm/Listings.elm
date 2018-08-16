module Listings exposing (view, listing_view)

import Html exposing (Html, div, text, ul, li, a, i, span)
import Html.Attributes exposing (href, class)
import Msgs exposing (Msg)
import Models exposing (Model, ListingId, Listing, PageContent, Route(..))
import Routing
import Common.ViewHelpers exposing (linkTo)


view : List Listing -> PageContent Msg
view listings =
    let
        title =
            text "Listings"

        content =
            div [ class "ui feed" ]
                (List.map listingElem listings)
    in
        { content = content
        , breadcrumbs = listingsBreadcrumbs
        , title = title
        }


listingsBreadcrumbs : List ( Route, String )
listingsBreadcrumbs =
    [ ( ListingsRoute, "Listings" ) ]


listingElem : Listing -> Html Msg
listingElem listing =
    let
        path =
            Routing.listingPath listing.id

        label =
            span [ class "ui green label" ] [ text "For Hire" ]

        title =
            span [] [ text listing.title ]

        date =
            div [ class "date" ] [ text "1 day ago" ]

        author =
            span [ class "author" ] [ text "Wiebe-Marten Wijnja" ]
    in
        linkTo path
            [ class "event listing-item" ]
            [ div [ class "content" ]
                [ div [ class "summary" ]
                    [ label
                    , text " "
                    , title
                    , date
                    ]
                , div [ class "meta" ]
                    [ rating_view listing.rating
                    , author
                    ]
                ]
            ]


listing_view : Listing -> PageContent Msg
listing_view listing =
    let
        label =
            span [ class "ui green label" ] [ text "For Hire" ]

        author =
            span [ class "author" ] [ text "Wiebe-Marten Wijnja" ]

        description =
                [ div [ class "ui header" ] [ text "Description" ]
                , div [] [ text ("A longer description of the freelance job will be placed here.") ]
                ]

        status =
                [ div [ class "ui header" ] [ text "Status" ]
                , div [] [ text "Status of fulfillment" ]
                ]

        timeline =
            div []
                [ div [ class "ui header" ] [ text "Timeline" ]
                , div [] [ text "Timeline here" ]
                ]

        rating =
            rating_view listing.rating
    in
        { title = span [] [ label, text " ", text listing.title ]
        , breadcrumbs = listingsBreadcrumbs ++ [ ( ListingRoute listing.id, listing.title ) ]
        , content =
            div [ class "ui stackable celled grid" ]
                [ div [ class "row" ]
                    [ div [ class "twelve wide column" ]
                        (description ++ status)
                    , div [ class "four wide column" ]
                        [ div [ class "ui header" ]
                            [ text "Author: "
                            , author
                            ]
                        , rating
                        , div [] [ text "Author info down here" ]
                        ]
                    ]
                , div [ class "row" ]
                    [ div [ class "sixteen wide column" ] [ timeline ]
                    ]
                ]
        }


rating_view : Int -> Html Msg
rating_view rat =
    div [ class "ui star rating" ]
        [ i [ class "ui icon active" ] []
        , i [ class "ui icon active" ] []
        , i [ class "ui icon active" ] []
        , i [ class "ui icon " ] []
        , i [ class "ui icon " ] []
        ]
