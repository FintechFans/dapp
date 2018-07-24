module Variadic exposing (..)

process args = \a -> process (args ++ [toString a])
