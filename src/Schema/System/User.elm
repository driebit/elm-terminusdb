module Schema.System.User exposing
    ( User
    , decode
    )

import Json.Decode as Decode exposing (Decoder)
import Schema.Prefix as Prefix
import Schema.System.Document as Document exposing (Document)
import Schema.Xsd as Xsd


type alias User =
    { id : String
    , type_ : String
    , comment : Document.TranslatedText
    , name : String
    }


decode : Prefix.Context -> Decoder User
decode context =
    let
        _ =
            Debug.log "context" context
    in
    Document.decodeType context Prefix.System "User" User
        |> Document.andMap (Document.field context Prefix.System "agent_name" (Xsd.string context))
