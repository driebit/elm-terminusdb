module Schema exposing
    ( prefixed
    , requireType
    )

import Json.Decode as Decode exposing (Decoder)
import Schema.Prefix as Prefix exposing (Prefix)


prefixed : (Prefix.Context -> Decoder value) -> Decoder value
prefixed decoder =
    Prefix.decode
        |> Decode.andThen decoder


requireType : Prefix.Context -> Prefix -> String -> Decoder String
requireType context prefix type_ =
    Decode.field "@type" Decode.string
        |> Decode.andThen (match context prefix type_)


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
