module EthAbi exposing (..)

import Char
import Hex


{-| TODO using BigInts where required
TODO testing!

Examples:

format_output = String.toList >> List.Extra.groupsOf 64 >> (List.map (String.fromList))

example1 = EthAbi.encode_args [Static (AbiUint 69), Static (AbiBool True)] |> format_output

example2 = EthAbi.encode_args [Static (AbiStaticBytes 3 "abc"), Static (AbiStaticBytes 3 "def")] |> format_output

example3 = EthAbi.encode_args [Dynamic (AbiBytes "dave"), Static (AbiBool True), Dynamic (AbiDynamicArray [Static (AbiUint 1), Static (AbiUint 2), Static (AbiUint 3)])] |> format_output

dynamic_example = EthAbi.encode_args [Static (AbiUint 291), Dynamic (AbiDynamicArray [Static (AbiUint 1110), Static (AbiUint 1929)]), Static (AbiStaticBytes 10 "1234567890"), Dynamic (AbiBytes "Hello, world!")] |> format_output

dynamic_example2 = EthAbi.encode_args [Dynamic (AbiDynamicArray [Dynamic (AbiDynamicArray [Static (AbiUint 1), Static (AbiUint 2)]), Dynamic (AbiDynamicArray [Static (AbiUint 3)])]), Dynamic (AbiDynamicArray [Dynamic (AbiString "one"), Dynamic (AbiString "two"), Dynamic (AbiString "three")])] |> format_output

-}
type AbiType
    = Static AbiStaticType
    | Dynamic AbiDynamicType


type
    AbiStaticType
    -- An unsigned (positive) integer:
    = AbiUint Int
      -- A signed integer:
    | AbiInt Int
      -- A boolean value:
    | AbiBool Bool
      --| A bytes array of known length; i.e bytes3, bytes10, etc:
    | AbiStaticBytes Int String
      --| An array of static length; all elements of the same type:
    | AbiStaticArray Int (List AbiStaticType)
      --| A tuple where none of the elements is a dynamic type:
    | AbiStaticTuple Int (List AbiStaticType)


type
    AbiDynamicType
    -- A bytes array of dynamic length:
    = AbiBytes String
      -- A string of dynamic length:
    | AbiString String
      -- An array with dynamic length; all elements of the same type:
    | AbiDynamicArray (List AbiType)
      -- An array with static length; all elements of the same type:
    | AbiArray Int (List AbiDynamicType)
      -- An tuple where one of the elements has to be Dynamic:
    | AbiDynamicTuple Int (List AbiType)


uint : Int -> Result String AbiStaticType
uint int =
    if int < 0 then
        Err "Negative integer cannot be converted to Unsigned Integer"
    else
        Ok (AbiUint int)



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
bytesInHexString str =
    String.length str // 2


dynamic_tail : AbiDynamicType -> String
dynamic_tail val =
    case val of
        AbiDynamicArray vals ->
            let
                len =
                    List.length vals
            in
                (static_encode (AbiUint len)) ++ encode_args (vals)

        AbiArray n_elems vals ->
            vals
                |> List.map (Dynamic)
                |> encode_args

        AbiBytes bytes ->
            let
                len =
                    String.length bytes

                lhs =
                    static_encode (AbiUint len)

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

        AbiDynamicTuple _ vals ->
            vals
                |> encode_args


padLeftTo32Bytes : Char -> String -> String
padLeftTo32Bytes char str =
    String.padLeft 64 char str


padRightTo32Bytes : Char -> String -> String
padRightTo32Bytes char str =
    String.padRight 64 char str


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
        AbiStaticArray len vals ->
            static_encode (AbiStaticTuple len vals)
 
        AbiStaticTuple _ vals ->
            vals
                |> List.map static_encode
                |> String.concat
