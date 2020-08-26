module Schema.Xsd exposing (string)

import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix


string : Prefix.Context -> Decoder String
string context =
    Decode.map2 always
        (Decode.field "@value" Decode.string)
        (Schema.requireType context Prefix.Xsd "string")
