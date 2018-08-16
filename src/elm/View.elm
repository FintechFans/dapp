module View exposing (..)

import Html exposing (Html, div, text, header, footer, span)
import Html.Attributes exposing (class)
import Msgs exposing (Msg)
import Models exposing (Model, Route, PageContent)
import Listings
import Dict
import Routing
import Common.ViewHelpers exposing (linkTo)


view : Model -> Html Msg
view model =
    div []
        [ header [] [ renderHeader model ]
        , div [class "content"] [ renderPageWrapper model ]
        , footer [] [ renderFooter ]
        ]


renderPageWrapper : Model -> Html Msg
renderPageWrapper model =
    div [class "ui container"]
        [ renderCurrentRoute model ]

renderHeader : Model -> Html Msg
renderHeader model = div []
         [ renderMenu model.ethereum_info
         ]

renderMenu : Models.EthereumInfo -> Html Msg
renderMenu ethereum_info = div [class "ui menu"]
             [ linkTo "/" [class "header item"] [text "FintechFans Decentralized Marketplace"]
             , linkTo "/" [class "item"] [text "Home"]
             , div [class "item"] [text <| "Ethereum Block Depth: " ++ ethereum_info.block_depth]
             ]

renderFooter : Html Msg
renderFooter = div [][]

renderCurrentRoute : Model -> Html Msg
renderCurrentRoute model =
    renderPage model.route <| case model.route of
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
                        listing
                            |> Listings.listing_view
        Models.NotFoundRoute ->
            notFoundView

page : String -> Html Msg -> Html Msg
page title content =
    div [class "pagewrapper"]
        [ div [class "ui header"] [text title]
        , div [class "pagewrapper-inner"] [content]
        ]

renderPage : Route -> PageContent Msg -> Html Msg
renderPage current_route page_content =
    div [class "pagewrapper"]
        [ div [class "ui breadcrumb"] (renderBreadcrumbs current_route page_content.breadcrumbs)
        , div [class "ui huge header"] [page_content.title]
        , div [class "pagewrapper-inner"] [page_content.content]
        ]

renderBreadcrumbs : Route -> List Models.Breadcrumb -> List (Html Msg)
renderBreadcrumbs current_route breadcrumbs=
    let
        breadcrumbHtml (route, crumb_name)=
            if route == current_route then
                div [class "active section"] [text crumb_name]
            else
                linkTo (Routing.toPath route) [class "section"] [text crumb_name]
    in
    breadcrumbs
        |> List.map breadcrumbHtml
           |> List.intersperse (span [class "divider"] [text "/"])

notFoundView : PageContent msg
notFoundView =
    let
        title = text "Not Found"
        breadcrumbs = [(Models.ListingsRoute, "Home")]
        content =
            div []
                [text "Error: Page not found. Please check the URL and try again"
                ]
    in
        { content = content
        , breadcrumbs = breadcrumbs
        , title = title
        }
