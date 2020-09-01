module Schema.Xsd exposing
    ( encodeNonNegativeInt
    , encodeString
    , string
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Schema
import Schema.Prefix as Prefix


string : Prefix.Context -> Decoder String
string context =
    Schema.requireType context Prefix.Xsd "string" <|
        Decode.field "@value" Decode.string


encodeString : String -> Encode.Value
encodeString value =
    Encode.object
        [ ( "@type", Encode.string "xsd:string" )
        , ( "@value", Encode.string value )
        ]


encodeNonNegativeInt : Int -> Encode.Value
encodeNonNegativeInt value =
    Encode.object
        [ ( "@type", Encode.string "xsd:nonNegativeInteger" )
        , ( "@value", Encode.int value )
        ]
