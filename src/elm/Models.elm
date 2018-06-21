module Models exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)

type alias Model =
    {route: Route
    , listings : (Dict String Listing)
    }

initialModel route =
    { route = route
    , listings = initial_listings
    }

initial_listings =
    [ {title= "For Hire", id= "0", rating= 2}
    , {title= "For Hire2", id= "1", rating= 2}
    , {title= "For Hire4", id= "2", rating= 2}
    , {title= "For Hire5", id= "3", rating= 2}
    ]
    |> List.map(\elem -> (elem.id, elem))
    |> Dict.fromList

type Route
    = ListingsRoute
      | ListingRoute ListingId
      | NotFoundRoute


type alias PageContent msg =
    { title : Html msg
    , breadcrumbs : List Breadcrumb
    , content : Html msg
    }

type alias Breadcrumb = (Route, String)

type alias ListingId = String

type alias Listing =
    { id: ListingId
    , title: String
    , rating: Int
    }
