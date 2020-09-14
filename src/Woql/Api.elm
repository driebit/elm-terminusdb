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

import Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Schema
import Schema.Prefix as Prefix exposing (Prefix)
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


buildAuthorizationToken : String -> String -> String
buildAuthorizationToken username password =
    Base64.encode (username ++ ":" ++ password)


branch =
    Cmd.none


clone =
    Cmd.none


connect : (Result Http.Error Session -> msg) -> ServerUrl -> Cmd msg
connect msg url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Basic " ++ buildAuthorizationToken "admin" "root") ]
        , url = Url.Builder.crossOrigin url [] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg (Schema.prefixed (sessionDecoder url))
        , timeout = Nothing
        , tracker = Nothing
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


query : (Result Http.Error Woql.Response -> msg) -> String -> String -> Maybe Woql.CommitInfo -> List Prefix -> Query -> Session -> Cmd msg
query msg org db maybeInfo p q session =
    let
        request =
            case maybeInfo of
                Nothing ->
                    Woql.QueryRequest p q

                Just i ->
                    Woql.QueryCommitRequest p q i
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Basic " ++ buildAuthorizationToken "admin" "root") ]
        , url = Url.Builder.crossOrigin session.server [ "woql", org, db, "local", "branch", "master" ] []
        , body = Http.jsonBody (Woql.request request)
        , expect = Http.expectJson msg (Woql.response session.context)
        , timeout = Nothing
        , tracker = Nothing
        }


rebase =
    Cmd.none


updateTriples =
    Cmd.none
