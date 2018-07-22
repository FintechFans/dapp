module EthAbi.Types exposing (..)

import BigInt exposing (BigInt)
import Result exposing (Result)


type Int256
    = Int256 BigInt


int : Int -> BigInt -> Result String Int256
int len integer =
    let
        ensureLenIsInRange len integer =
            if len > 0 && len <= 256 && len % 8 == 0 then
                Ok integer
            else
                Err ("length should be in range 0..256 and be a multiple of 8, but it is " ++ (toString len))
    in
        integer
            |> ensureLenIsInRange len
            |> Result.andThen (ensureIntegerFits (len - 1))
            |> Result.map (Int256)


-- TODO: Constructor shorthand for every `int<M>`?
int8 =
    int 8


int16 =
    int 16


int32 =
    int 32


int64 =
    int 64


int128 =
    int 128

int256 =
    int 256

int256ToBigInt (Int256 big_int) = big_int

type UInt256
    = UInt256 BigInt


uint : Int -> BigInt -> Result String UInt256
uint len integer =
    let
        ensureLenIsInRange len integer =
            if len > 0 && len <= 256 && len % 8 == 0 then
                Ok integer
            else
                Err ("length should be in range 0..256 and be a multiple of 8, but it is " ++ (toString len))

        ensurePositive integer =
            if BigInt.gte integer (BigInt.fromInt 0) then
                Ok integer
            else
                Err ("UInt256 called with a negative value: " ++ toString integer)
    in
        integer
            |> ensureLenIsInRange len
            |> Result.andThen ensurePositive
            |> Result.andThen (ensureIntegerFits len)
            |> Result.map (UInt256)

-- TODO: Constructor shorthand for every `uint<M>`?
uint8 =
    uint 8


uint16 =
    uint 16


uint32 =
    uint 32


uint64 =
    uint 64


uint128 =
    uint 128

uint256 =
    uint 256

uint256ToBigInt (UInt256 big_int) = big_int

ensureIntegerFits : Int -> BigInt -> Result String BigInt
ensureIntegerFits len integer =
    let
        len_bits len =
            BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt len)
    in
        if BigInt.lte (BigInt.abs integer) (len_bits len) then
            Ok integer
        else
            Err ("Integer too large to fit in " ++ toString len ++ " bits")


type Bytes32
    = Bytes32 String


bytes : Int -> String -> Result String Bytes32
bytes len str =
    let
        ensureLenIsInRange len str =
            if len > 0 && len <= 32 then
                Ok str
            else
                Err ("Bytes length should be in range 0..32, but it is " ++ (toString len))

        -- TODO
        ensureStringFits str =
            if String.length str <= len then
                Ok (Bytes32 str)
            else
                Err "String is too large to fit in a Bytes32."
    in
        str
            |> ensureLenIsInRange len
            |> Result.andThen ensureStringFits


-- TODO: Constructor shorthand for every `bytes<M>`?
bytes32 =
    bytes 32
