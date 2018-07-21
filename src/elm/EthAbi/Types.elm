module EthAbi.Types exposing (..)

import BigInt exposing (BigInt)
import Result exposing (Result)


type Int128
    = Int128 BigInt


int : Int -> BigInt -> Result String Int128
int len integer =
    let
        ensureLenIsPowerOfTwo len integer =
            if List.member len [ 8, 16, 32, 64, 128 ] then
                Ok integer
            else
                Err ("Int128 length should be a multiple of 2 and between 8 and 128, but is " ++ (toString len))
    in
        integer
            |> ensureLenIsPowerOfTwo len
            |> Result.andThen (ensureIntegerFits len)
            |> Result.map (Int128)


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


int128ToBigInt (Int128 big_int) = big_int

type UInt256
    = UInt256 BigInt


uint : Int -> BigInt -> Result String UInt256
uint len integer =
    let
        ensureLenIsPowerOfTwo len integer =
            if List.member len [ 8, 16, 32, 64, 128, 256 ] then
                Ok integer
            else
                Err ("UInt256 length should be a multiple of 2 and between 8 and 128, but is " ++ (toString len))

        ensurePositive integer =
            if BigInt.gte integer (BigInt.fromInt 0) then
                Ok integer
            else
                Err ("UInt256 called with a negative value: " ++ toString integer)
    in
        integer
            |> ensureLenIsPowerOfTwo len
            |> Result.andThen ensurePositive
            |> Result.andThen (ensureIntegerFits len)
            |> Result.map (UInt256)

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
        if BigInt.gte (BigInt.abs integer) (len_bits len) then
            Ok integer
        else
            Err ("Integer too large to fit in " ++ toString len ++ " bits")


type Bytes32
    = Bytes32 String


bytes : Int -> String -> Result String Bytes32
bytes len str =
    let
        ensureLenIsPowerOfTwo len str =
            if List.member len [ 2, 4, 8, 16, 32 ] then
                Ok str
            else
                Err ("Bytes length should be a multiple of two, but it is " ++ (toString len))

        -- TODO
        ensureStringFits str =
            if String.length str <= len then
                Ok (Bytes32 str)
            else
                Err "String is too large to fit in a Bytes32."
    in
        str
            |> ensureLenIsPowerOfTwo len
            |> Result.andThen ensureStringFits


bytes2 =
    bytes 2


bytes4 =
    bytes 4


bytes8 =
    bytes 8


bytes16 =
    bytes 16


bytes32 =
    bytes 32
