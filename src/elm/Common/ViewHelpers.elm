module Common.ViewHelpers exposing (..)

import Routing
import Html exposing (Html, div, text, ul, li, a)
import Html.Attributes exposing (href)

linkTo path attributes content = a ([href path, Routing.linkTo path] ++ attributes) (content)
