module Schema.System.User exposing
    ( User
    , decode
    )

import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix
import Schema.System.Document as Document exposing (Document)
import Schema.Xsd as Xsd


type alias User =
    { id : String
    , type_ : String
    , comment : Schema.TranslatedText
    , name : String
    }


decode : Prefix.Context -> Decoder User
decode context =
    Document.decodeType context Prefix.System "User" User
        |> Schema.andMap (Schema.field context Prefix.System "agent_name" (Xsd.string context))
