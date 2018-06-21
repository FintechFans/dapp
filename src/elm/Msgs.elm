module Msgs exposing (..)

import Navigation exposing (Location)

type Msg
    = Foo
    | Bar
    | NavigateTo String
    | LocationChange Location
