module Schema.Xsd exposing (string)

import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix


string : Prefix.Context -> Decoder String
string context =
    Schema.requireType context Prefix.Xsd "string" <|
        Decode.field "@value" Decode.string
