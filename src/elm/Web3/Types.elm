module Web3.Types exposing (..)

import Porter

type alias Message msg_type = Porter.Msg String String msg_type
