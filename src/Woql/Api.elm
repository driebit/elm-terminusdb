module Woql.Api exposing
    ( Config
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
import Schema
import Schema.System.User as User exposing (User)
import Url.Builder


type alias Config =
    { server : String
    }


branch =
    Cmd.none


clone =
    Cmd.none


connect : (Result Http.Error User -> msg) -> Config -> Cmd msg
connect msg config =
    Http.get
        { url = Url.Builder.crossOrigin config.server [] []
        , expect = Http.expectJson msg (Schema.prefixed User.decode)
        }


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


query =
    Cmd.none


rebase =
    Cmd.none


updateTriples =
    Cmd.none
