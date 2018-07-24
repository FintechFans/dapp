module EthAbi.Internal exposing (..)

{-| Internal module; exposing everything to this library, but is not in the public packages, so outside of the EthAbi library its contents are not visible.
-}

{-| Ensures that `condition value`, is `True`.
Returns a Result depending on the outcome.
 -}
ensure : (a -> Bool) -> e -> a -> Result e a
ensure condition error value =
    if (condition value) then
        Ok value
    else
        Err error

type Hexstring
    = Hexstring String

type Bytes32
    = Bytes32 String
