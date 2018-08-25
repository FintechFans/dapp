module Semantic exposing (..)

import Html exposing (Html, div, text, footer, span)
import Html.Attributes exposing (class)

icon : String -> Html msg
icon iconName =
    Html.i [ class (iconName ++ " icon") ] []

header : List (Html.Attribute msg) -> List (Html msg) -> Html msg
header attrs html = div (attrs ++ [class "ui header"]) html

menu : List (Html.Attribute msg) -> List (Html msg) -> Html msg
menu attrs html = div (attrs ++ [class "ui menu"]) html

container : List (Html.Attribute msg) -> List (Html msg) -> Html msg
container attrs html = div (attrs ++ [class "ui container"]) html
