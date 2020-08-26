module Main exposing (..)

import Browser
import Html exposing (Html)
import Http
import Schema.System.User exposing (User)
import Woql.Api


type alias Model =
    { config : Woql.Api.Config
    , state : State
    }


type State
    = Connecting
    | Connected User
    | WithError String


type alias Flags =
    {}


type Msg
    = GotConnected (Result Http.Error User)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        config =
            { server = "http://127.0.0.1:6363"
            }
    in
    ( { config = config
      , state = Connecting
      }
    , Woql.Api.connect GotConnected config
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotConnected (Ok user) ->
            let
                _ =
                    Debug.log "user" user
            in
            ( { model | state = Connected user }, Cmd.none )

        GotConnected (Err reason) ->
            let
                error =
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
            ( { model | state = WithError error }, Cmd.none )


view : Model -> Html Msg
view { state } =
    case state of
        Connecting ->
            Html.text "Connecting.."

        Connected user ->
            Html.text <| "Connected " ++ user.name

        WithError message ->
            Html.text <| "Error " ++ message
