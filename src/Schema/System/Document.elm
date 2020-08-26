module Schema.System.Document exposing
    ( Document
    , TranslatedText
    , andMap
    , decode
    , decodeType
    , field
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix exposing (Prefix)
import Tuple


type alias Document =
    { id : String
    , type_ : String
    , comment : TranslatedText
    }


type alias TranslatedText =
    Dict String String


andMap : Decoder a -> Decoder (a -> value) -> Decoder value
andMap =
    Decode.map2 (|>)


decode : Prefix.Context -> Decoder Document
decode context =
    Decode.map3 Document
        (Decode.field "@id" Decode.string)
        (Decode.field "@type" Decode.string)
        (field context Prefix.Rdfs "comment" translatedText)


decodeType : Prefix.Context -> Prefix -> String -> (String -> String -> TranslatedText -> value) -> Decoder value
decodeType context prefix typeName documentType =
    Decode.map3 documentType
        (Decode.field "@id" Decode.string)
        (Schema.requireType context prefix typeName)
        (field context Prefix.Rdfs "comment" translatedText)


field : Prefix.Context -> Prefix -> String -> Decoder value -> Decoder value
field context prefix typeName decoder =
    Prefix.fromContext context (Prefix.uri prefix)
        |> List.map (\p -> Decode.field (p ++ ":" ++ typeName) decoder)
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
