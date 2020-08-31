module Schema.System.User exposing
    ( User
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix
import Schema.System.Document as Document exposing (Document)
import Schema.Xsd as Xsd


type alias User =
    { id : String
    , comment : Schema.TranslatedText
    , name : String
    }


decoder : Prefix.Context -> Decoder User
decoder context =
    Document.decoderFor context Prefix.System "User" User
        |> Schema.andMap (Schema.field context Prefix.System "agent_name" (Xsd.string context))
