module Schema.Prefix exposing
    ( Context
    , Prefix(..)
    , decode
    , fromContext
    , uri
    , uriFromContext
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias Context =
    Dict String String


type Prefix
    = Api
    | None
    | Owl
    | Rdf
    | Rdfs
    | Scm
    | System
    | Xsd


decode : Decoder Context
decode =
    Decode.field "@context" (Decode.dict Decode.string)


uri : Prefix -> String
uri prefix =
    case prefix of
        Api ->
            "http://terminusdb.com/schema/api"

        None ->
            ""

        Owl ->
            "http://www.w3.org/2002/07/owl#"

        Rdf ->
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

        Rdfs ->
            "http://www.w3.org/2000/01/rdf-schema#"

        Scm ->
            "http://195.201.12.87:6365/Seshat/schema#"

        System ->
            "http://terminusdb.com/schema/system#"

        Xsd ->
            "http://www.w3.org/2001/XMLSchema#"


uriFromContext : Context -> String -> String
uriFromContext context prefix =
    Dict.get prefix context
        |> Maybe.withDefault prefix


fromContext : Context -> String -> List String
fromContext context u =
    Dict.foldl
        (\k v p ->
            if v == u then
                k :: p

            else
                p
        )
        [ u ]
        context
