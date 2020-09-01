module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Html
import Http
import Schema exposing (Value(..))
import Schema.Prefix exposing (Prefix(..))
import Schema.System.User exposing (User)
import Woql
import Woql.Api exposing (Session)
import Woql.Query exposing (..)


type Model
    = Connecting
    | Connected Session
    | ConnectedWithError String Session
    | Error String


type alias Flags =
    {}


type Msg
    = GotConnected (Result Http.Error Session)
    | PerformQuery (List Prefix) Query
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
    , Woql.Api.connect GotConnected "http://127.0.0.1:6363"
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

        PerformQuery prefixes query ->
            case model of
                Connected session ->
                    ( model, Woql.Api.query GotQueryResponse Nothing prefixes query session )

                _ ->
                    ( model, Cmd.none )

        GotQueryResponse (Ok result) ->
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
                        (PerformQuery [ Scm ] <|
                            Select
                                [ "Start", "Start_Label", "End", "End_Label" ]
                                (And
                                    [ Triple (Subject (Var "Journey")) (Predicate (Node "rdf:type")) (Object (Node "scm:Journey"))
                                    , Triple (Subject (Var "Journey")) (Predicate (Node "scm:start_station")) (Object (Var "Start"))
                                    , Optional (Triple (Subject (Var "Start")) (Predicate (Node "rdfs:label")) (Object (Var "Start_Label")))
                                    , Optional (Triple (Subject (Var "End")) (Predicate (Node "rdfs:label")) (Object (Var "End_Label")))
                                    , Triple (Subject (Var "Journey")) (Predicate (Node "scm:bicycle")) (Object (Var "Bike"))
                                    ]
                                )
                        )
                    ]
                    [ Html.text "Bikes query" ]
                , Html.button
                    [ Html.onClick
                        (PerformQuery [ Scm ] <|
                            Triple (Subject (Var "Journey")) (Predicate (Node "rdf:type")) (Object (Node "scm:Journey"))
                        )
                    ]
                    [ Html.text "Simplest query" ]
                ]

        ConnectedWithError message _ ->
            Html.text <| "Error " ++ message

        Error message ->
            Html.text <| "Error " ++ message
