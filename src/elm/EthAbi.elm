module EthAbi exposing (..)

import Hex
import Char

{-|

TODO using BigInts where required
TODO testing!

 -}


type AbiType
    = Static AbiStaticType
    | Dynamic AbiDynamicType


type AbiStaticType
    = AbiUint Int
    | AbiInt Int
    | AbiBool Bool
    | AbiStaticBytes Int String
    | AbiStaticTuple (List AbiStaticType)


type AbiDynamicType
    = AbiBytes String
    | AbiString String
    | AbiDynamicArray (List AbiType)
    | AbiArray Int (List AbiDynamicType)
    | AbiDynamicTuple (List AbiType) -- ?


{-
 32 bytes == 64 hexadecimal characters
 This means that when counting hexchars, we get twice the number as when counting bytes.
 -}
encode_args : List AbiType -> String
encode_args args =
    let
        concat_tuple ( a, b ) =
            String.concat [ a, b ]

        n_args =
            List.length args

        head_length =
            n_args * 32

        encode_elem elem ( prev_head, prev_tail ) =
            Debug.log "encode_elem"
                (case elem of
                    Static val ->
                        ( prev_head ++ (static_encode val), prev_tail )

                    Dynamic val ->
                        let
                            dynamic_address =
                                head_length + (bytesInHexString prev_tail)
                        in
                            ( prev_head ++ (static_encode (AbiUint dynamic_address)), prev_tail ++ dynamic_tail val )
                )
    in
        args
            |> List.foldl encode_elem ( "", "" )
            |> concat_tuple

bytesInHexString : String -> Int
bytesInHexString str = String.length str // 2

dynamic_tail : AbiDynamicType -> String
dynamic_tail val =
    case val of
        AbiDynamicArray vals ->
            let
                len =
                    List.length vals
            in
                encode_args ([ Static (AbiUint len) ] ++ vals)

        AbiArray n_elems vals ->
            vals
                |> List.map (Dynamic)
                |> encode_args

        AbiBytes bytes ->
            let
                len =
                    String.length bytes
                lhs = static_encode (AbiUint len)
                rhs =
                    bytes
                        |> strToBytes
                        |> String.padRight 64 '0'
            in
                lhs ++ rhs

        AbiString str ->
            str
                |> AbiBytes
                |> dynamic_tail

        AbiDynamicTuple vals ->
            vals
                |> encode_args

padLeftTo32Bytes : Char -> String -> String
padLeftTo32Bytes char str = String.padLeft 64 char str

padRightTo32Bytes : Char -> String -> String
padRightTo32Bytes char str = String.padRight 64 char str

strToBytes : String -> String
strToBytes str =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""


static_encode : AbiStaticType -> String
static_encode val =
    case val of
        AbiUint int ->
            int
                |> Hex.toString
                |> padLeftTo32Bytes '0'

        AbiInt int ->
            let
                hexint =
                    Hex.toString int
            in
                if String.startsWith "-" hexint then
                    hexint
                        |> String.dropLeft 1
                        |> padLeftTo32Bytes 'f'
                else
                    hexint
                        |> padLeftTo32Bytes '0'

        AbiBool bool ->
            if bool then
                static_encode (AbiUint 1)
            else
                static_encode (AbiUint 0)

        AbiStaticBytes n_elems bytes ->
            bytes
                |> strToBytes
                |> padRightTo32Bytes '0'

        AbiStaticTuple vals ->
            vals
                |> List.map static_encode
                |> String.concat
