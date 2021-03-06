module Schema.Prefix exposing
    ( Context
    , Prefix(..)
    , context
    , decodeContext
    , encodeContext
    , fromContext
    , string
    , uri
    , uriFromContext
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Context =
    Dict String String


type Prefix
    = Api
    | Doc
    | None
    | Owl
    | Rdf
    | Rdfs
    | Scm
    | System
    | Woql
    | Xsd


context : List Prefix -> Context
context =
    List.foldl
        (\p a -> Dict.insert (string p) (uri p) a)
        Dict.empty


string : Prefix -> String
string prefix =
    case prefix of
        Api ->
            "api"

        Doc ->
            "doc"

        None ->
            ""

        Owl ->
            "owl"

        Rdf ->
            "rdf"

        Rdfs ->
            "rdfs"

        Scm ->
            "scm"

        System ->
            "system"

        Woql ->
            "woql"

        Xsd ->
            "xsd"


decodeContext : Decoder Context
decodeContext =
    Decode.field "@context" (Decode.dict Decode.string)
        |> Decode.andThen
            (\c ->
                Decode.succeed (Dict.insert "api" (uri Api) c)
            )


encodeContext : Context -> Encode.Value
encodeContext =
    Encode.dict identity Encode.string


uri : Prefix -> String
uri prefix =
    case prefix of
        Api ->
            "http://terminusdb.com/schema/api"

        Doc ->
            "terminusdb:///data/"

        None ->
            ""

        Owl ->
            "http://www.w3.org/2002/07/owl#"

        Rdf ->
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

        Rdfs ->
            "http://www.w3.org/2000/01/rdf-schema#"

        Scm ->
            "terminusdb:///schema#"

        System ->
            "http://terminusdb.com/schema/system#"

        Woql ->
            "http://terminusdb.com/schema/woql#"

        Xsd ->
            "http://www.w3.org/2001/XMLSchema#"


uriFromContext : Context -> String -> String
uriFromContext c prefix =
    Dict.get prefix c
        |> Maybe.withDefault prefix


fromContext : Context -> String -> List String
fromContext c u =
    Dict.foldl
        (\k v p ->
            if v == u then
                k :: p

            else
                p
        )
        [ u ]
        c
