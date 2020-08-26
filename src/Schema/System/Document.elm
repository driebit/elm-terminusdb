module Schema.System.Document exposing
    ( Document
    , decode
    , decodeType
    )

import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix exposing (Prefix)
import Tuple


type alias Document =
    { id : String
    , type_ : String
    , comment : Schema.TranslatedText
    }


decode : Prefix.Context -> Decoder Document
decode context =
    Decode.map3 Document
        (Decode.field "@id" Decode.string)
        (Decode.field "@type" Decode.string)
        (Schema.field context Prefix.Rdfs "comment" Schema.translatedText)


decodeType : Prefix.Context -> Prefix -> String -> (String -> String -> Schema.TranslatedText -> value) -> Decoder value
decodeType context prefix typeName documentType =
    Decode.map3 documentType
        (Decode.field "@id" Decode.string)
        (Schema.requireType context prefix typeName)
        (Schema.field context Prefix.Rdfs "comment" Schema.translatedText)
