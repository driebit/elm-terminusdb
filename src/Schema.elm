module Schema exposing
    ( TranslatedText
    , Value(..)
    , andMap
    , field
    , prefixed
    , requireType
    , translatedText
    , value
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Schema.Prefix as Prefix exposing (Prefix)


type Value
    = StringLiteral String
    | IntLiteral Int
    | TextLiteral TranslatedText
    | Reference Prefix String


type alias TranslatedText =
    Dict String String


andMap : Decoder a -> Decoder (a -> value) -> Decoder value
andMap =
    Decode.map2 (|>)


prefixed : (Prefix.Context -> Decoder value) -> Decoder value
prefixed decoder =
    Prefix.decodeContext
        |> Decode.andThen decoder


requireType : Prefix.Context -> Prefix -> String -> Decoder a -> Decoder a
requireType context prefix type_ decoder =
    always
        decoder
        (Decode.field "@type" Decode.string
            |> Decode.andThen (match context prefix type_)
        )


match : Prefix.Context -> Prefix -> String -> String -> Decoder String
match context expectedPrefix expectedType type_ =
    let
        expectedPrefixUri =
            Prefix.uri expectedPrefix

        ( actualPrefix, actualType ) =
            prefixAndType type_

        actualPrefixUri =
            Prefix.uriFromContext context actualPrefix
    in
    if (expectedPrefixUri == actualPrefixUri) && (expectedType == actualType) then
        Decode.succeed (actualPrefix ++ ":" ++ actualType)

    else
        Decode.fail
            ("Expected "
                ++ expectedPrefixUri
                ++ ":"
                ++ expectedType
                ++ ", but got "
                ++ actualPrefixUri
                ++ ":"
                ++ actualType
            )


prefixAndType : String -> ( String, String )
prefixAndType type_ =
    case String.split ":" type_ of
        p :: t :: _ ->
            ( p, t )

        t :: [] ->
            ( "", t )

        [] ->
            ( "", "" )


field : Prefix.Context -> Prefix -> String -> Decoder a -> Decoder a
field context prefix name decoder =
    Prefix.fromContext context (Prefix.uri prefix)
        |> List.map
            (\p ->
                if p == "" then
                    Decode.field name decoder

                else
                    Decode.field (p ++ ":" ++ name) decoder
            )
        |> Decode.oneOf


value : Prefix.Context -> Prefix -> String -> a -> Decoder a
value context prefix name instance =
    Prefix.fromContext context (Prefix.uri prefix)
        |> List.map
            (\p ->
                Decode.string
                    |> Decode.andThen
                        (\s ->
                            if s == (p ++ ":" ++ name) then
                                Decode.succeed instance

                            else
                                Decode.fail "Value does not match"
                        )
            )
        |> Decode.oneOf


translatedText : Decoder TranslatedText
translatedText =
    let
        textDecoder =
            Decode.map2 Tuple.pair
                (Decode.field "@language" Decode.string)
                (Decode.field "@value" Decode.string)
    in
    Decode.map Dict.fromList <|
        Decode.oneOf
            [ Decode.andThen (\t -> Decode.succeed [ t ]) textDecoder
            , Decode.list textDecoder
            ]
