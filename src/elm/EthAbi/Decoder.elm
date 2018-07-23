module EthAbi.Decoder
    exposing
        ( run
        , succeed
        , fail
        , map
          -- , andThen
        , map2
        , apply
        , int256
        , uint256
        , bool
        , static_bytes
        , array
        , static_array -- TODO remove
        , dynamic_array -- TODO remove
        , run_keeping_leftover -- TODO remove
        )

import Char
import Array exposing (Array)
import Hex
import List.Extra
import Result.Extra
import BigInt exposing (BigInt)
import EthAbi.Types exposing (Int256, UInt256, Bytes32)


-- Some types are Dynamic, and this propagates up through complex types
-- (i.e. the statically sized array of a dynamic type is also dynamic.)


type AbiParamModifier
    = Static
    | Dynamic


{-| On successful decoding, we return a 't', the unparsed rest of the string, and the offset from the beginning of parsing, in groups of 32-bytes.
This last step is used to properly offset the calculations for dynamic types.
-}
type DecodingResult t
    = DecodingResult t String Int


mapDecodingResult fun (DecodingResult val hexstr offset) =
    DecodingResult (fun val) hexstr offset


type alias EthAbiDecoder t =
    ( AbiParamModifier, String -> Int -> Result String (DecodingResult t) )


succeed : a -> EthAbiDecoder a
succeed val =
    ( Static, \hexstr offset -> Ok (DecodingResult val hexstr offset) )


fail : String -> EthAbiDecoder a
fail error_message =
    ( Static, \_ _ -> Err error_message )


map : (a -> b) -> EthAbiDecoder a -> EthAbiDecoder b
map fun ( abi_param_modifier, decoderfun ) =
    ( abi_param_modifier
    , \hexstr offset ->
        (decoderfun hexstr offset) |> Result.map (mapDecodingResult fun)
    )



-- TODO does modifier get propagated here properly?


andThen : (a -> EthAbiDecoder b) -> EthAbiDecoder a -> EthAbiDecoder b
andThen fun ( modifier, decoderfun ) =
    let
        compoundfun =
            \hexstring offset ->
                case decoderfun hexstring offset of
                    Err err ->
                        Err err

                    Ok (DecodingResult res hexstring_rest new_offset) ->
                        run_keeping_leftover (fun res) hexstring_rest new_offset
    in
        ( modifier, compoundfun )


run : EthAbiDecoder t -> String -> Result String t
run ( modifier, decoder ) hexstring =
    let
        ensureValidResult result =
            case result of
                DecodingResult result "" _ ->
                    Ok result

                DecodingResult _ hexstring_leftover _ ->
                    Err ("At end of parsing had some hexstring left: " ++ hexstring_leftover)
    in
        (decoder hexstring 0)
            |> Result.andThen ensureValidResult


run_keeping_leftover : EthAbiDecoder t -> String -> Int -> Result String (DecodingResult t)
run_keeping_leftover ( modifier, decoder ) hexstring offset =
    let
        x =
            (toString ( ( modifier, decoder ), hexstring, offset ))
    in
        decoder hexstring offset



{- TODO this function probably is not that useful,
   since both decoders use the same input text.

   It would make more sense to have `b` run on `a`'s leftover input.
-}
{- Used to run two decoders one after the other, on the same hexstring, and combine their results. -}


map2 : (a -> b -> c) -> EthAbiDecoder a -> EthAbiDecoder b -> EthAbiDecoder c
map2 fun ( ma, da ) ( mb, db ) =
    let
        modifier =
            case ( ma, mb ) of
                ( Static, Static ) ->
                    Static

                ( _, _ ) ->
                    Dynamic

        mapped_fun =
            \hexstring offset ->
                case da hexstring offset of
                    Err err ->
                        Err err

                    Ok (DecodingResult res leftover_hexstr new_offset) ->
                        run_keeping_leftover (map (fun res) ( mb, db )) leftover_hexstr new_offset
    in
        ( modifier, mapped_fun )


{-| Used to run multiple decoders one after the other, on the same hexstring.

This is used to parse tuples, as well as to parse multiple arguments that are passed to a function
(according to the ABI documentation, fun(a,b,c) should be parsed as fun((a,b,c,)) so it is the same)

The following are identical ways to do the same thing:

       succeed (,) |> apply int |> apply uint

       map2 (,) int uint

-}
apply : EthAbiDecoder a -> EthAbiDecoder (a -> b) -> EthAbiDecoder b
apply =
    (flip << map2) (<|)


int256 : EthAbiDecoder Int256
int256 =
    let
        twosComplementPow =
            BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 255)

        twosComplement bigint =
            if BigInt.gte bigint twosComplementPow then
                BigInt.sub bigint (BigInt.mul (BigInt.fromInt 2) twosComplementPow)
            else
                bigint
    in
        ( Static
        , \hexstr offset ->
            hexstr
                |> (withFirst32Bytes offset)
                    (hexToBigInt
                        >> Result.map twosComplement
                        >> Result.andThen EthAbi.Types.int256
                    )
        )


uint256 : EthAbiDecoder UInt256
uint256 =
    ( Static
    , \hexstr offset ->
        hexstr
            |> (withFirst32Bytes offset)
                (hexToBigInt
                    >> Result.andThen EthAbi.Types.uint256
                )
    )


{-| Only to be used inside this module, inside a dynamic decoder;
- Will trim results to fit in one int8 (in an Int),
-}
unsafe_int8 : EthAbiDecoder Int
unsafe_int8 =
    ( Dynamic
    , \hexstr offset ->
        hexstr
            |> (withFirst32Bytes offset)
                (Hex.fromString)
    )


bool : EthAbiDecoder Bool
bool =
    let
        intToBool num =
            case num of
                0 ->
                    Ok False

                1 ->
                    Ok True

                _ ->
                    Err "Impossible to convert ABI-encoded value to boolean, not '0' or '1'"
    in
        ( Static
        , \hexstr offset ->
            hexstr
                |> (withFirst32Bytes offset)
                    (Hex.fromString
                        >> Result.andThen intToBool
                    )
        )


static_bytes : Int -> EthAbiDecoder Bytes32
static_bytes len =
    ( Static
    , \hexstr offset ->
        hexstr
            |> (withFirst32Bytes offset)
                ((trimBytesRight len)
                    >> bytesToStr
                    >> Result.andThen (EthAbi.Types.bytes len)
                )
    )


array : Int -> EthAbiDecoder elem -> EthAbiDecoder (Array elem)
array len ( me, de ) =
    case me of
        Static ->
            static_array len ( me, de )

        Dynamic ->
            dynamic_array ( me, de )


static_array : Int -> EthAbiDecoder elem -> EthAbiDecoder (Array elem)
static_array len ( me, de ) =
    let
        arr =
            Array.repeat len ( me, de )

        accum_fun : EthAbiDecoder elem -> EthAbiDecoder (Array elem) -> EthAbiDecoder (Array elem)
        accum_fun elem acc =
            apply elem (map (flip Array.push) acc)
    in
        arr |> Array.foldl (accum_fun) (succeed Array.empty)


dynamic_array : EthAbiDecoder elem -> EthAbiDecoder (Array elem)
dynamic_array elem_decoder =
    let
        ensureHexstringSize hexstr =
            if String.length hexstr >= 64 then
                Ok hexstr
            else
                Err ("Given ABI hexadecimal string is not at least 64 bytes")

        res_fun =
            unsafe_int8
                |> andThen
                    (\calculated_offset_from_start ->
                        ( Dynamic
                        , (\new_hexstr previous_offset ->
                            let
                                dynamic_arr_tail_decoder =
                                    unsafe_int8
                                        |> andThen (\len -> static_array (Debug.log "dynamic_arr_tail_decoder len " len) elem_decoder)

                                hexstr_tail =
                                    String.dropLeft (64 * offset_to_array) (Debug.log "new_hexstr" new_hexstr)

                                offset_to_array =
                                    Debug.log "offset_to_array" (calculated_offset_from_start - previous_offset)

                                decoded_dynamic_arr =
                                    run_keeping_leftover dynamic_arr_tail_decoder hexstr_tail 0 |> Debug.log ("decoded_dynamic_arr")

                                hexstr_head =
                                    String.left (64 * offset_to_array) new_hexstr
                            in
                                decoded_dynamic_arr
                                    |> Result.map
                                        (\(DecodingResult res hexstr_tail_rest offset) ->
                                            DecodingResult res (Debug.log ("combined hexstr") (hexstr_head ++ hexstr_tail_rest)) (Debug.log ("combined offsets") (previous_offset + offset))
                                        )
                          )
                        )
                    )

        -- \hexstring offset ->
        --         |> andThen uint256
        --         -- |> ensureHexstringSize
        --         -- |> Result.map (stringTakeFirst 64)
        --         |> Debug.crash "TODO"
    in
        res_fun


trimBytesLeft : Int -> String -> String
trimBytesLeft len str =
    String.dropLeft (64 - (2 * len)) str


trimBytesRight : Int -> String -> String
trimBytesRight len str =
    String.dropRight (64 - (2 * len)) str


hexToBigInt : String -> Result String BigInt
hexToBigInt hex =
    BigInt.fromString ("0x" ++ hex) |> Result.fromMaybe "Could not parse hex as BigInt"


bytesToStr : String -> Result String String
bytesToStr bytes =
    let
        -- two hexchars -> 0..255 -> char
        byteToChar =
            (Hex.fromString >> Result.map (Char.fromCode))
    in
        bytes
            |> stringGroupsOf 2
            |> List.map byteToChar
            |> Result.Extra.combine
            |> Result.map String.fromList


stringGroupsOf : Int -> String -> List String
stringGroupsOf num str =
    str
        |> String.toList
        |> List.Extra.groupsOf num
        |> List.map String.fromList



-- TODO does it make more sense to return a Maybe here?


stringTakeFirst : Int -> String -> Result String ( String, String )
stringTakeFirst num str =
    case stringGroupsOf num str of
        [] ->
            Err ("ABI hexstring too short; expected at least " ++ (toString num) ++ " characters")

        head :: tail ->
            Ok ( head, String.join "" tail )


withFirst32Bytes : Int -> (String -> Result String a) -> String -> Result String (DecodingResult a)
withFirst32Bytes offset fun str =
    str
        |> stringTakeFirst 64
        |> Result.andThen
            (\( vala, valb ) ->
                case fun vala of
                    Err err ->
                        Err err

                    Ok res ->
                        Ok (DecodingResult res valb (offset + 1))
            )


ensureSingleWord : String -> Result String String
ensureSingleWord hexstr =
    if String.length hexstr == 64 then
        Ok hexstr
    else
        Err "Given ABI hexadecimal string is not 256 bits"



-- tupleTODO =
--     Decode.map2
