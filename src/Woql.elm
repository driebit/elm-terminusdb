module Woql exposing
    ( CommitInfo
    , Request(..)
    , Response
    , Status(..)
    , request
    , response
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Schema
import Schema.Prefix as Prefix exposing (Prefix)
import Woql.Query exposing (Query)


type alias CommitInfo =
    { author : String
    , message : String
    }


commitInfo : CommitInfo -> Encode.Value
commitInfo { author, message } =
    Encode.object [ ( "author", Encode.string author ), ( "message", Encode.string message ) ]


type alias Response =
    { status : Status
    , variables : List String
    , bindings : List String
    , inserts : Int
    , deletes : Int
    , retries : Int
    }


type Status
    = Success
    | Failure


response : Prefix.Context -> Decoder Response
response context =
    Schema.requireType context Prefix.Api "WoqlResponse" <|
        Decode.map6 Response
            (Schema.field context
                Prefix.Api
                "status"
                (Decode.oneOf
                    [ Schema.value context Prefix.Api "success" Success
                    , Schema.value context Prefix.Api "failure" Failure
                    ]
                )
            )
            (Schema.field context Prefix.Api "variable_names" (Decode.list Decode.string))
            (Schema.field context Prefix.None "bindings" (Decode.list Decode.string))
            (Decode.field "inserts" Decode.int)
            (Decode.field "deletes" Decode.int)
            (Decode.field "transaction_retry_count" Decode.int)


type Request
    = QueryRequest (List Prefix) Query
    | QueryCommitRequest (List Prefix) Query CommitInfo


request : Request -> Encode.Value
request r =
    case r of
        QueryRequest p q ->
            Encode.object
                [ ( "query", Woql.Query.encode p q )
                ]

        QueryCommitRequest p q c ->
            Encode.object
                [ ( "commit_info", commitInfo c )
                , ( "query", Woql.Query.encode p q )
                ]
