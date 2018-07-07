module EthAbi exposing (..)


type AbiType
    = Static AbiStaticType
    | Dynamic AbiDynamicType


type AbiStaticType
    = AbiUint Int
    | AbiInt Int
    | AbiBool Bool
    | AbiStaticTuple (List AbiStaticType)


type AbiDynamicType
    = AbiBytes String
    | AbiString String
    | AbiDynamicArray (List AbiType)
    | AbiArray Int (List AbiDynamicType)
    | AbiDynamicTuple (List AbiType) -- ?


encode : AbiType -> String
encode abi_val = case abi_val of
                 Static val ->
                     static_encode val
                 Dynamic val ->
                     Debug.crash "bar"

static_encode : AbiStaticType -> String
static_encode val =
    case val of
        AbiUint int -> toString int
        AbiInt int -> toString int
        AbiBool bool -> toString bool
        AbiStaticTuple vals ->
            vals
            |> List.map static_encode
            |> String.concat

dynamic_encode val =
    case val of
        AbiBytes str -> str
        AbiString str -> str
        AbiDynamicArray vals ->
            vals
            |> List.map encode
            |> String.concat
        AbiArray n_elems vals ->
            vals
                |> List.map dynamic_encode
                |> String.concat
        AbiDynamicTuple vals ->
            vals
            |> List.map encode
            |> String.concat

strlen = String.length
byteslen = String.length
