module Models exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)

import Eth.Types
import Msgs exposing (Msg)
import Web3.Types

type alias Model =
    { route: Route
    , listings : (Dict String Listing)
    , ethereum_info : EthereumInfo
    , web3_porter : Web3.Types.Model Msg
    }

type alias EthereumInfo =
    { provider : Eth.Types.HttpProvider
    , block_depth : String
    }

initialModel : Route -> Model
initialModel route =
    { route = route
    , listings = initial_listings
    , ethereum_info = initial_ethereum_info
    , web3_porter = Web3.Types.init
    }

initial_listings : Dict String Listing
initial_listings =
    [ {title = "For Hire" , id = "0", rating = 2}
    , {title = "For Hire2", id = "1", rating = 2}
    , {title = "For Hire4", id = "2", rating = 2}
    , {title = "For Hire5", id = "3", rating = 2}
    ]
    |> List.map(\elem -> (elem.id, elem))
    |> Dict.fromList

initial_ethereum_info : EthereumInfo
initial_ethereum_info =
    {provider = "https://ropsten.infura.io/" --"http://37.97.139.47:8549"--"http://api.myetherapi.com/rop"
    , block_depth = "???"
    }

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
