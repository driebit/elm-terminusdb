module Main exposing (..)

import Browser
import Dict
import Html exposing (Html)
import Html.Events as Html
import Http
import Schema exposing (Value(..))
import Schema.Prefix exposing (Prefix(..))
import Schema.System.User exposing (User)
import Woql exposing (Bindings)
import Woql.Api exposing (Session)
import Woql.Query exposing (..)


type Model
    = Connecting
    | Connected Session
    | ConnectedWithResult Bindings Session
    | ConnectedWithError String Session
    | Error String


type alias Flags =
    {}


type Msg
    = GotConnected (Result Http.Error Session)
    | PerformQuery String String (List Prefix) Query
    | GotQueryResponse (Result Http.Error Woql.Response)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Connecting
    , Woql.Api.connect GotConnected { server = "https://localhost:6363", user = "admin", password = "root" }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        errorString reason =
            case reason of
                Http.BadUrl url ->
                    "Bad url: " ++ url

                Http.Timeout ->
                    "Network timeout"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus status ->
                    "Bad status: " ++ String.fromInt status

                Http.BadBody body ->
                    "Bad body: " ++ body
    in
    case msg of
        GotConnected (Ok session) ->
            let
                _ =
                    Debug.log "session" session
            in
            ( Connected session, Cmd.none )

        GotConnected (Err reason) ->
            ( Error <| errorString reason, Cmd.none )

        PerformQuery org db prefixes query ->
            case model of
                Connected session ->
                    ( model, Woql.Api.query GotQueryResponse org db Nothing prefixes query session )

                _ ->
                    ( model, Cmd.none )

        GotQueryResponse (Ok result) ->
            case model of
                Connected session ->
                    ( ConnectedWithResult result.bindings session, Cmd.none )

                ConnectedWithResult _ session ->
                    ( ConnectedWithResult result.bindings session, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotQueryResponse (Err reason) ->
            ( Error <| errorString reason, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Connecting ->
            Html.text "Connecting.."

        Connected session ->
            Html.div []
                [ Html.text <| "Connected " ++ session.user.name
                , Html.button
                    [ Html.onClick
                        (PerformQuery "admin" "driebit_test" [ Scm, Rdf, Rdfs ] <|
                            Select
                                [ "Start", "Start_Label", "End", "End_Label" ]
                                (And
                                    [ Triple (Var "Journey") (Node "rdf:type") (Node "scm:Journey")
                                    , Triple (Var "Journey") (Node "scm:start_station") (Var "Start")
                                    , Optional (Triple (Var "Start") (Node "rdfs:label") (Var "Start_Label"))
                                    , Triple (Var "Journey") (Node "scm:start_station") (Var "Start")
                                    , Optional (Triple (Var "End") (Node "rdfs:label") (Var "End_Label"))
                                    , Triple (Var "Journey") (Node "scm:bicycle") (Var "Bike")
                                    ]
                                )
                        )
                    ]
                    [ Html.text "Bikes query" ]
                , Html.button
                    [ Html.onClick
                        (PerformQuery "admin" "driebit_test" [ Scm, Rdf, Rdfs ] <|
                            And
                                [ Triple (Var "Journey") (Node "rdf:type") (Node "scm:Journey")
                                , Triple (Var "Journey") (Node "scm:start_station") (Var "Start")
                                , Optional (Triple (Var "Start") (Node "rdfs:label") (Var "Start_Label"))
                                , Triple (Var "Journey") (Node "scm:journey_bicycle") (Var "Bike")
                                ]
                        )
                    ]
                    [ Html.text "Simplest query" ]
                , Html.button
                    [ Html.onClick
                        (PerformQuery "admin" "system" [ System, Rdf, Rdfs ] <|
                            And
                                [ Triple (Var "User") (Node "rdf:type") (Node "system:User")
                                , Optional (Triple (Var "User") (Node "rdfs:label") (Var "User_Name"))
                                ]
                        )
                    ]
                    [ Html.text "System users" ]
                , Html.button
                    [ Html.onClick
                        (PerformQuery "admin" "driebit_site" [ System, Rdf, Rdfs ] <|
                            And
                                [ Triple (Var "Page") (Node "rdf:type") (Node "driebit:Page")
                                , Optional (Triple (Var "User") (Node "rdfs:label") (Var "User_Name"))
                                ]
                        )
                    ]
                    [ Html.text "Register Page doctype" ]
                , Html.button
                    [ Html.onClick
                        (PerformQuery "admin" "driebit_site" [ System, Rdf, Rdfs, Scm ] <|
                            And
                                [ Triple (Var "Page") (Node "rdf:type") (Var "Type")
                                ]
                        )
                    ]
                    [ Html.text "List pages" ]
                ]

        ConnectedWithResult bindings _ ->
            case bindings of
                first :: _ ->
                    Html.table []
                        (Html.tr [] (List.map (Html.th [] << List.singleton << Html.text) (Dict.keys first))
                            :: List.map
                                (\b ->
                                    Html.tr [] (List.map (Html.td [] << List.singleton << Html.text) (Dict.values b))
                                )
                                bindings
                        )

                [] ->
                    Html.text "empty result"

        ConnectedWithError message _ ->
            Html.text <| "Error " ++ message

        Error message ->
            Html.text <| "Error " ++ message
