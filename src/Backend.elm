module Backend exposing (..)

import AssocList
import AssocSet
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import RPC
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { sessions = AssocList.empty }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        ClientDisconnected _ clientId ->
            ( { model
                | sessions =
                    AssocList.map
                        (\_ session -> { session | connections = AssocSet.remove clientId session.connections })
                        model.sessions
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        LoadSessionRequest sessionName ->
            case AssocList.get sessionName model.sessions of
                Just session ->
                    ( { model
                        | sessions =
                            AssocList.insert
                                sessionName
                                { session
                                    | connections = AssocSet.insert clientId session.connections
                                }
                                model.sessions
                      }
                    , Lamdera.sendToFrontend clientId (LoadSessionResponse session.history)
                    )

                Nothing ->
                    let
                        session : Session
                        session =
                            { history = []
                            , connections = AssocSet.singleton clientId
                            }
                    in
                    ( { model | sessions = AssocList.insert sessionName session model.sessions }
                    , Lamdera.sendToFrontend clientId (LoadSessionResponse session.history)
                    )

        ResetSessionRequest ->
            case getSessionByClientId clientId model of
                Just ( sessionName, session ) ->
                    let
                        model2 =
                            { model
                                | sessions =
                                    AssocList.insert
                                        sessionName
                                        { session | history = [] }
                                        model.sessions
                            }
                    in
                    ( model2
                    , RPC.broadcastToClients sessionName ResetSession model2
                    )

                Nothing ->
                    ( model, Cmd.none )


getSessionByClientId : ClientId -> BackendModel -> Maybe ( SessionName, Session )
getSessionByClientId clientId model =
    AssocList.toList model.sessions
        |> List.Extra.find (\( sessionName, session ) -> AssocSet.member clientId session.connections)
