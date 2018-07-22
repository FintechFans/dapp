module EthAbi.Decoder
    exposing
        ( run
        , decode
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
        , static_array
        , stringTakeFirst
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


type alias EthAbiDecoder t =
    ( AbiParamModifier, String -> Result String ( t, String ) )


succeed : a -> EthAbiDecoder a
succeed val =
    ( Static, \hexstr -> Ok ( val, hexstr ) )


fail : String -> EthAbiDecoder a
fail error_message =
    ( Static, \_ -> Err error_message )


map : (a -> b) -> EthAbiDecoder a -> EthAbiDecoder b
map fun ( abi_param_modifier, decoderfun ) =
    ( abi_param_modifier, decoderfun >> Result.map (Tuple.mapFirst fun) )



-- TODO does modifier get propagated here properly?


andThen : (a -> EthAbiDecoder b) -> EthAbiDecoder a -> EthAbiDecoder b
andThen fun ( modifier, decoderfun ) =
    let
        compoundfun =
            \hexstring ->
                case decoderfun hexstring of
                    Err err ->
                        Err err

                    Ok ( res, hexstring_rest ) ->
                        run_keeping_leftover (fun res) hexstring_rest
    in
        ( modifier, compoundfun )


run : EthAbiDecoder t -> String -> Result String t
run ( modifier, decoder ) hexstring =
    let
        ensureValidResult result =
            case result of
                ( result, "" ) ->
                    Ok result

                ( _, hexstring_leftover ) ->
                    Err ("At end of parsing had some hexstring left: " ++ hexstring_leftover)
    in
        hexstring
            |> decoder
            |> Result.andThen ensureValidResult


run_keeping_leftover ( modifier, decoder ) hexstring =
    decoder hexstring



{- TODO this function probably is not that useful,
   since both decoders use the same input text.

   It would make more sense to have `b` run on `a`'s leftover input.
-}


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
            \hexstring ->
                case da hexstring of
                    Err err ->
                        Err err

                    Ok ( res, leftover_hexstr ) ->
                        run_keeping_leftover (map (fun res) ( mb, db )) leftover_hexstr
    in
        ( modifier, mapped_fun )



{- Used to create types for tuple elements -}


apply =
    map2 (|>)



-- da
-- |> andThen (\resa -> db |> map (fun resa))


decode : a -> EthAbiDecoder a
decode =
    succeed



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
        , \hexstr ->
            hexstr
                |> withFirst32Bytes
                    (hexToBigInt
                        >> Result.map twosComplement
                        >> Debug.log "twoscomplement"
                        >> Result.andThen EthAbi.Types.int256
                    )
        )


uint256 : EthAbiDecoder UInt256
uint256 =
    ( Static
    , \hexstr ->
        hexstr
            |> withFirst32Bytes
                (hexToBigInt
                    >> Result.andThen EthAbi.Types.uint256
                )
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
        , \hexstr ->
            hexstr
                |> withFirst32Bytes
                    (Hex.fromString
                        >> Result.andThen intToBool
                    )
        )


static_bytes : Int -> EthAbiDecoder Bytes32
static_bytes len =
    ( Static
    , \hexstr ->
        hexstr
            |> withFirst32Bytes
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
            dynamic_array len ( me, de )


static_array : Int -> EthAbiDecoder elem -> EthAbiDecoder (Array elem)
static_array len ( me, de ) =
    let
        ensureHexstringSize hexstr =
            -- TODO: Based on element size?
            if String.length hexstr == 64 * len then
                Ok hexstr
            else
                Err ("Given ABI hexadecimal string is not 32 * " ++ toString len ++ " bytes long")

        foofun : EthAbiDecoder elem -> EthAbiDecoder (Array elem) -> EthAbiDecoder (Array elem)
        foofun elem acc =
            apply elem (map (flip Array.push) acc)

        arr =
            Array.repeat len ( me, de )

        res =
            arr |> Array.foldl (foofun) (succeed Array.empty)

        -- \hexstring ->
        --     hexstring
        -- |> ensureHexstringSize
        -- |> Result.map (stringGroupsOf ((String.length hexstring) // len))
        -- |> Result.andThen (List.map (de >> (Result.map Tuple.first)) >> Result.Extra.combine)
        -- |> Result.map Array.fromList
        -- |> Result.map (\res -> ( res, "TODO" ))
    in
        res


dynamic_array : Int -> EthAbiDecoder elem -> EthAbiDecoder (Array elem)
dynamic_array len ( me, de ) =
    let
        ensureHexstringSize hexstr =
            if String.length hexstr >= 64 then
                Ok hexstr
            else
                Err ("Given ABI hexadecimal string is not at least 64 bytes")

        res_fun =
            \hexstring ->
                hexstring
                    |> ensureHexstringSize
                    |> Result.map (stringTakeFirst 64)
                    |> Debug.crash "TODO"
    in
        ( me, res_fun )


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


withFirst32Bytes : (String -> Result String a) -> String -> Result String ( a, String )
withFirst32Bytes fun str =
    str
        |> stringTakeFirst 64
        |> Result.andThen
            (\( vala, valb ) ->
                case fun vala of
                    Err err ->
                        Err err

                    Ok res ->
                        Ok ( res, valb )
            )


ensureSingleWord : String -> Result String String
ensureSingleWord hexstr =
    if String.length hexstr == 64 then
        Ok hexstr
    else
        Err "Given ABI hexadecimal string is not 256 bits"



-- tupleTODO =
--     Decode.map2
