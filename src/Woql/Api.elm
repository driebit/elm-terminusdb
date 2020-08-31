module Woql.Api exposing
    ( Session
    , branch
    , clone
    , connect
    , createDatabase
    , createGraph
    , deleteDatabase
    , deleteGraph
    , fetch
    , getTriples
    , pull
    , push
    , query
    , rebase
    , updateTriples
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix
import Schema.System.User as User exposing (User)
import Url.Builder
import Woql
import Woql.Query exposing (Query)


type alias Session =
    { server : ServerUrl
    , context : Prefix.Context
    , user : User
    }


type alias ServerUrl =
    String


branch =
    Cmd.none


clone =
    Cmd.none


connect : (Result Http.Error Session -> msg) -> ServerUrl -> Cmd msg
connect msg url =
    Http.get
        { url = Url.Builder.crossOrigin url [] []
        , expect = Http.expectJson msg (Schema.prefixed (sessionDecoder url))
        }


sessionDecoder : ServerUrl -> Prefix.Context -> Decoder Session
sessionDecoder url context =
    Decode.map (Session url context)
        (User.decoder context)


createDatabase =
    Cmd.none


createGraph =
    Cmd.none


deleteDatabase =
    Cmd.none


deleteGraph =
    Cmd.none


fetch =
    Cmd.none


getTriples =
    Cmd.none


pull =
    Cmd.none


push =
    Cmd.none


query : (Result Http.Error Woql.Response -> msg) -> Maybe Woql.CommitInfo -> Query -> Session -> Cmd msg
query msg maybeInfo q session =
    let
        request =
            case maybeInfo of
                Nothing ->
                    Woql.QueryRequest q

                Just i ->
                    Woql.QueryCommitRequest q i
    in
    Http.post
        { url = Url.Builder.crossOrigin session.server [ "woql" ] []
        , body = Http.jsonBody (Woql.request request)
        , expect = Http.expectJson msg (Woql.response session.context)
        }


rebase =
    Cmd.none


updateTriples =
    Cmd.none
