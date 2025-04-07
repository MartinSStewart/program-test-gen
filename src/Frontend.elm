port module Frontend exposing (addEvent, app, codegen, newCode, parseCode, parseErrorToString)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict
import Elm.CodeGen as Codegen
import Elm.Parser
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Env
import File.Download
import Html
import Html.Attributes
import Icons
import JsCode
import Json.Decode
import Json.Encode
import Lamdera exposing (ClientId)
import List.Extra
import Maybe.Extra
import Pretty
import Random
import SessionName
import Sha256
import Task
import Types exposing (..)
import Ui
import Ui.Anim
import Ui.Events
import Ui.Font
import Ui.Input
import Ui.Prose
import Url
import Url.Parser


port write_file_to_js : String -> Cmd msg


port write_file_from_js : (Bool -> msg) -> Sub msg


port select_file_to_js : () -> Cmd msg


port select_file_from_js : ({ name : String, content : String } -> msg) -> Sub msg


port got_file_api_not_supported : (() -> msg) -> Sub msg


port get_file_api_not_supported : () -> Cmd msg


port set_overscroll : Bool -> Cmd msg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onMouseUp (Json.Decode.succeed MouseUpEvent)
                    , select_file_from_js GotFile
                    , got_file_api_not_supported (\() -> FileApiNotSupportedFromPort)
                    , write_file_from_js WroteToFile
                    ]
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse Url.Parser.string url of
        Just sessionName ->
            ( LoadingSession
                { key = key
                , sessionName = SessionName.fromString sessionName
                }
            , Cmd.batch
                [ get_file_api_not_supported ()
                , set_overscroll False
                , Lamdera.sendToBackend (LoadSessionRequest (SessionName.fromString sessionName))
                ]
            )

        Nothing ->
            ( LoadingSession { key = key, sessionName = SessionName.fromString "" }
            , Cmd.batch
                [ get_file_api_not_supported ()
                , set_overscroll False
                , Random.generate
                    GotRandomSessionName
                    (Random.map
                        (\int -> String.fromInt int |> Sha256.sha224 |> String.left 16 |> SessionName.fromString)
                        (Random.int Random.minInt Random.maxInt)
                    )
                ]
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        LoadingSession loading ->
            case msg of
                GotRandomSessionName sessionName ->
                    ( LoadingSession { loading | sessionName = sessionName }
                    , Cmd.batch
                        [ Lamdera.sendToBackend (LoadSessionRequest sessionName)
                        , Browser.Navigation.replaceUrl loading.key ("/" ++ SessionName.toString sessionName)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        LoadedSession loaded ->
            let
                ( newLoaded, cmd ) =
                    updateLoaded msg loaded
            in
            ( LoadedSession newLoaded, cmd )


setEventVisibility : Int -> Bool -> LoadedData -> ( LoadedData, Cmd frontendMsg )
setEventVisibility index isHidden model =
    case Array.get index model.history of
        Just event ->
            if event.isHidden == isHidden then
                ( model, Cmd.none )

            else
                ( { model | history = Array.set index { event | isHidden = isHidden } model.history }
                , SetEventVisibilityRequest { index = index, isHidden = isHidden } |> Lamdera.sendToBackend
                )

        Nothing ->
            ( model, Cmd.none )


updateLoaded : FrontendMsg -> LoadedData -> ( LoadedData, Cmd FrontendMsg )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        PressedResetSession ->
            ( model, Lamdera.sendToBackend ResetSessionRequest )

        GotRandomSessionName _ ->
            ( model, Cmd.none )

        ScrolledToBottom ->
            ( model, Cmd.none )

        MouseDownOnEvent index isHidden ->
            setEventVisibility index isHidden { model | mouseDownOnEvent = True }

        MouseEnterOnEvent index isHidden ->
            if model.mouseDownOnEvent then
                setEventVisibility index isHidden model

            else
                ( model, Cmd.none )

        PressedCommitToFile ->
            case model.parsedCode of
                ParseSuccess ok ->
                    let
                        settings : Settings
                        settings =
                            model.settings

                        text : String
                        text =
                            codegen
                                ok
                                { settings | showAllCode = True }
                                (Array.toList model.history |> List.filter (\event -> not event.isHidden))
                    in
                    ( { model | commitStatus = Committing text }, write_file_to_js text )

                _ ->
                    ( model, Cmd.none )

        ElmUiMsg msg2 ->
            ( { model | elmUiState = Ui.Anim.update ElmUiMsg msg2 model.elmUiState }, Cmd.none )

        ToggledIncludeScreenPos bool ->
            updateSettings (\settings -> { settings | includeScreenPos = bool }) model

        ToggledIncludeClientPos bool ->
            updateSettings (\settings -> { settings | includeClientPos = bool }) model

        ToggledIncludePagePos bool ->
            updateSettings (\settings -> { settings | includePagePos = bool }) model

        MouseUpEvent ->
            ( { model | mouseDownOnEvent = False }, Cmd.none )

        PressedEvent ->
            ( model, Cmd.none )

        GotFile { content } ->
            ( { model
                | parsedCode =
                    case parseCode content of
                        Ok ok ->
                            ParseSuccess ok

                        Err err ->
                            ParseFailed err
              }
            , Cmd.none
            )

        PressedSelectFile ->
            ( model, select_file_to_js () )

        PressedNewFile ->
            ( { model | parsedCode = ParseSuccess newCode }
            , Cmd.none
            )

        FileApiNotSupportedFromPort ->
            ( { model | parsedCode = FileApiNotSupported }, Cmd.none )

        ToggledShowAllCode bool ->
            updateSettings (\settings -> { settings | showAllCode = bool }) model

        WroteToFile isSuccessful ->
            case model.commitStatus of
                Committing text ->
                    if isSuccessful then
                        ( { model
                            | history = Array.empty
                            , commitStatus = CommitSuccess
                            , parsedCode =
                                case parseCode text of
                                    Ok ok ->
                                        ParseSuccess ok

                                    Err _ ->
                                        model.parsedCode
                          }
                        , Lamdera.sendToBackend ResetSessionRequest
                        )

                    else
                        ( { model | commitStatus = CommitFailed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedDownloadTestGen ->
            ( model, File.Download.string "test-gen.js" "" (JsCode.code (SessionName.toString model.sessionName) Env.domain) )


updateSettings : (Settings -> Settings) -> LoadedData -> ( LoadedData, Cmd frontendMsg )
updateSettings updateFunc model =
    let
        settings2 =
            updateFunc model.settings
    in
    ( { model | settings = settings2 }, SetSettingsRequest settings2 |> Lamdera.sendToBackend )


addEvent : Event -> { a | history : Array Event } -> { a | history : Array Event }
addEvent event model =
    let
        event2 : Event
        event2 =
            { event
                | isHidden =
                    case event.eventType of
                        KeyUp keyEvent ->
                            shouldHideKeyEvent keyEvent

                        KeyDown keyEvent ->
                            shouldHideKeyEvent keyEvent

                        _ ->
                            False
            }

        history2 : Array Event
        history2 =
            case Array.get (Array.length model.history - 1) model.history of
                Just last ->
                    if event2.timestamp - last.timestamp < 0 then
                        Array.push event2 model.history
                            |> Array.toList
                            |> List.sortBy .timestamp
                            |> Array.fromList

                    else
                        Array.push event2 model.history

                Nothing ->
                    Array.push event2 model.history
    in
    { model
        | history =
            Array.foldr
                (\item state ->
                    case state.latestEvent of
                        Just { timestamp, previous } ->
                            if timestamp - item.timestamp > 2000 then
                                state

                            else
                                { latestEvent =
                                    Just
                                        { timestamp = timestamp
                                        , previous =
                                            if item.isHidden then
                                                previous

                                            else
                                                item
                                        }
                                , array =
                                    case ( previous.eventType, item.eventType, previous.clientId == item.clientId ) of
                                        ( Input _, Input _, True ) ->
                                            Array.set state.index { item | isHidden = True } state.array

                                        _ ->
                                            state.array
                                , index = state.index - 1
                                }

                        Nothing ->
                            { state | latestEvent = Just { timestamp = item.timestamp, previous = item }, index = state.index - 1 }
                )
                { index = Array.length history2 - 1, latestEvent = Nothing, array = history2 }
                history2
                |> .array
    }


shouldHideKeyEvent : KeyEvent -> Bool
shouldHideKeyEvent keyEvent =
    (String.length keyEvent.key == 1 || keyEvent.key == "Shift" || keyEvent.key == "Backspace")
        && not keyEvent.altKey
        && not keyEvent.ctrlKey
        && not keyEvent.metaKey


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        LoadSessionResponse events ->
            case model of
                LoadingSession loading ->
                    ( LoadedSession
                        { key = loading.key
                        , sessionName = loading.sessionName
                        , history = events.history
                        , copyCounter = 0
                        , elmUiState = Ui.Anim.init
                        , settings = events.settings
                        , mouseDownOnEvent = False
                        , parsedCode = WaitingOnUser
                        , commitStatus = NotCommitted
                        , noEventsHaveArrived = Array.isEmpty events.history
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SessionUpdate event ->
            case model of
                LoadedSession loaded ->
                    ( addEvent event { loaded | commitStatus = NotCommitted, noEventsHaveArrived = False }
                        |> LoadedSession
                    , Browser.Dom.setViewportOf eventsListContainer 0 99999
                        |> Task.attempt (\_ -> ScrolledToBottom)
                    )

                LoadingSession _ ->
                    ( model, Cmd.none )

        ResetSession ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = Array.empty }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


parseCodeHelper : String -> Int -> Int -> Result ParseError ParsedCode
parseCodeHelper code httpRequestsStart testsStart =
    let
        fromListIndices : List Int
        fromListIndices =
            String.indexes "|> Dict.fromList" code

        testsEndIndex : Maybe Int
        testsEndIndex =
            case List.Extra.find (\index -> index > testsStart) (String.indexes "\n    ]" code) of
                Just index ->
                    Just index

                Nothing ->
                    case List.Extra.find (\index -> index > testsStart) (String.indexes "\n    []" code) of
                        Just index ->
                            index + String.length "\n    [" |> Just

                        Nothing ->
                            Nothing
    in
    case
        ( List.Extra.find (\index -> index > httpRequestsStart) fromListIndices
        , testsEndIndex
        )
    of
        ( Just httpRequestsEnd, Just testsEnd ) ->
            let
                httpRequestsResult =
                    String.slice httpRequestsStart httpRequestsEnd code |> parseHttpRequests

                sorted =
                    List.sortBy
                        (\( a, _, _ ) -> a)
                        [ ( httpRequestsStart, httpRequestsEnd, HttpRequestCode )
                        , ( testsEnd, testsEnd, TestEntryPoint )
                        ]

                last : Int
                last =
                    case List.reverse sorted of
                        ( _, end, _ ) :: _ ->
                            end

                        [] ->
                            String.length code
            in
            case httpRequestsResult of
                Ok httpRequests ->
                    { codeParts =
                        List.foldl
                            (\( start, end, codeType ) state ->
                                { codeParts =
                                    codeType
                                        :: UserCode (String.slice state.previousIndex start code)
                                        :: state.codeParts
                                , previousIndex = end
                                }
                            )
                            { previousIndex = 0, codeParts = [] }
                            sorted
                            |> .codeParts
                            |> (\a -> UserCode (String.slice last (String.length code) code) :: a)
                            |> List.reverse
                    , httpRequests = httpRequests
                    , noPriorTests =
                        case String.slice testsStart testsEnd code |> String.split "\n    [" of
                            [ _, rest ] ->
                                String.contains "," rest |> not

                            _ ->
                                False
                    }
                        |> Ok

                Err () ->
                    Err InvalidHttpRequests

        ( Nothing, _ ) ->
            Err HttpRequestsEndNotFound

        ( _, Nothing ) ->
            Err TestEntryPointNotFound


parseCode : String -> Result ParseError ParsedCode
parseCode code =
    case ( String.indexes "\nhttpRequests =" code, String.indexes "\ntests : " code ) of
        ( [ httpRequestsStart ], [ testsStart ] ) ->
            parseCodeHelper code httpRequestsStart testsStart

        ( [ _ ], [] ) ->
            Err TestEntryPointNotFound

        _ ->
            Err UnknownError


newCode : ParsedCode
newCode =
    { codeParts =
        [ """module MyTests exposing (main, setup, tests)

import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Effect.Browser.Dom as Dom
import Effect.Lamdera
import Effect.Test as T exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..), PointerOptions(..))
import Frontend
import Json.Decode
import Json.Encode
import Test.Html.Query
import Test.Html.Selector as Selector
import Time
import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
import Url exposing (Url)


setup : T.ViewerWith (List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    T.viewerWith tests
        |> T.addBytesFiles (Dict.values httpRequests)


main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    T.startViewer setup


domain : Url
domain =
    { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


{-| Please don't modify or rename this function -}
httpRequests : Dict String String"""
            |> String.replace "\u{000D}" ""
            |> UserCode
        , HttpRequestCode
        , """|> Dict.fromList


handleHttpRequests : Dict String Bytes -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
handleHttpRequests httpData { currentRequest } =
    case Dict.get (currentRequest.method ++ "_" ++ currentRequest.url) httpRequests of
        Just filepath ->
            case Dict.get filepath httpData of
                Just data ->
                    BytesHttpResponse { url = currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty } data

                Nothing ->
                    UnhandledHttpRequest

        Nothing ->
            UnhandledHttpRequest


{-| You can change parts of this function represented with `...`.
The rest needs to remain unchanged in order for the test generator to be able to add new tests.

    tests : ... -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
    tests ... =
        let
            config = ...

            ...
        in
        [ ...
        ]
-}
tests : Dict String Bytes -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests httpData =
    let
        config =
            T.Config
                Frontend.app_
                Backend.app_
                (handleHttpRequests httpData)
                (\\_ -> Nothing)
                (\\_ -> UnhandledFileUpload)
                (\\_ -> UnhandledMultiFileUpload)
                domain
    in
    ["""
            |> String.replace "\u{000D}" ""
            |> UserCode
        , TestEntryPoint
        , UserCode "\n    ]"
        ]
    , httpRequests = []
    , noPriorTests = True
    }


parseHttpRequests : String -> Result () (List ( String, String ))
parseHttpRequests code =
    case Elm.Parser.parseToFile ("module A exposing (..)\n" ++ code) of
        Ok ast ->
            case ast.declarations of
                [ Node _ (FunctionDeclaration func) ] ->
                    case Node.value func.declaration |> .expression of
                        Node _ (ListExpr requests) ->
                            List.filterMap
                                (\(Node _ request) ->
                                    case request of
                                        TupledExpression [ Node _ (Literal a), Node _ (Literal b) ] ->
                                            Just ( a, b )

                                        _ ->
                                            Nothing
                                )
                                requests
                                |> Ok

                        _ ->
                            Err ()

                _ ->
                    Err ()

        Err _ ->
            Err ()


eventsListContainer : String
eventsListContainer =
    "eventsListContainer"


type alias MillisecondWaitBefore =
    Int


type EventType2
    = Input2 ClientId MillisecondWaitBefore { targetId : Maybe String, text : String }
    | Click2 ClientId MillisecondWaitBefore { targetId : Maybe String }
    | ClickLink2 ClientId MillisecondWaitBefore LinkEvent
    | Connect2 ClientId MillisecondWaitBefore ConnectEvent (List EventType2)
    | KeyUp2 ClientId MillisecondWaitBefore KeyEvent
    | KeyDown2 ClientId MillisecondWaitBefore KeyEvent
    | PointerDown2 ClientId MillisecondWaitBefore PointerEvent
    | PointerUp2 ClientId MillisecondWaitBefore PointerEvent
    | PointerMove2 ClientId MillisecondWaitBefore PointerEvent
    | PointerLeave2 ClientId MillisecondWaitBefore PointerEvent
    | PointerCancel2 ClientId MillisecondWaitBefore PointerEvent
    | PointerOver2 ClientId MillisecondWaitBefore PointerEvent
    | PointerEnter2 ClientId MillisecondWaitBefore PointerEvent
    | PointerOut2 ClientId MillisecondWaitBefore PointerEvent
    | TouchStart2 ClientId MillisecondWaitBefore TouchEvent
    | TouchCancel2 ClientId MillisecondWaitBefore TouchEvent
    | TouchMove2 ClientId MillisecondWaitBefore TouchEvent
    | TouchEnd2 ClientId MillisecondWaitBefore TouchEvent
    | FromJsPort2 ClientId MillisecondWaitBefore { port_ : String, data : String }
    | WindowResize2 ClientId MillisecondWaitBefore WindowResizeEvent
    | CheckView2 ClientId MillisecondWaitBefore CheckViewEvent
    | MouseDown2 ClientId MillisecondWaitBefore MouseEvent
    | MouseUp2 ClientId MillisecondWaitBefore MouseEvent
    | MouseMove2 ClientId MillisecondWaitBefore MouseEvent
    | MouseLeave2 ClientId MillisecondWaitBefore MouseEvent
    | MouseOver2 ClientId MillisecondWaitBefore MouseEvent
    | MouseEnter2 ClientId MillisecondWaitBefore MouseEvent
    | MouseOut2 ClientId MillisecondWaitBefore MouseEvent
    | Focus2 ClientId MillisecondWaitBefore FocusEvent
    | Blur2 ClientId MillisecondWaitBefore BlurEvent
    | Wheel2 ClientId MillisecondWaitBefore WheelEvent


eventsToEvent2Helper state =
    Maybe.Extra.toList state.previousEvent
        ++ state.rest
        |> List.reverse
        |> List.map .eventType


eventsToEvent2 :
    { previousEvent : Maybe { eventType : EventType2, time : Int }, rest : List { eventType : EventType2, time : Int } }
    -> Int
    -> List Event
    -> List EventType2
eventsToEvent2 ({ previousEvent, rest } as state) startTime events =
    case events of
        [] ->
            eventsToEvent2Helper { previousEvent = previousEvent, rest = rest }

        { clientId, eventType, timestamp } :: events2 ->
            let
                delay : Int
                delay =
                    case previousEvent of
                        Just { time } ->
                            timestamp - time

                        Nothing ->
                            timestamp - startTime
            in
            case eventType of
                Input input ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = Input2 clientId delay input, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                KeyDown keyDown ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = KeyDown2 clientId delay keyDown, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                KeyUp keyUp ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = KeyUp2 clientId delay keyUp, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerDown a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerDown2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerUp a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerUp2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerMove a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerMove2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerLeave a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerLeave2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerCancel a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerCancel2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerOver a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerOver2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerEnter a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerEnter2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                PointerOut a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = PointerOut2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                TouchStart a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = TouchStart2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                TouchCancel a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = TouchCancel2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                TouchMove a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = TouchMove2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                TouchEnd a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = TouchEnd2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                Connect connect ->
                    { previousEvent =
                        Just
                            { eventType =
                                Connect2
                                    clientId
                                    delay
                                    connect
                                    (eventsToEvent2 { previousEvent = Nothing, rest = [] } timestamp events2)
                            , time = timestamp
                            }
                    , rest = Maybe.Extra.toList previousEvent ++ rest
                    }
                        |> eventsToEvent2Helper

                Click mouseEvent ->
                    eventsToEvent2
                        { previousEvent =
                            Just
                                { eventType = Click2 clientId delay { targetId = mouseEvent.targetId }
                                , time = timestamp
                                }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                ClickLink linkEvent ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = ClickLink2 clientId delay linkEvent, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                Http _ ->
                    eventsToEvent2 state startTime events2

                HttpLocal _ ->
                    eventsToEvent2 state startTime events2

                Paste pasteEvent ->
                    eventsToEvent2
                        { previousEvent =
                            Just
                                { eventType =
                                    Input2
                                        clientId
                                        delay
                                        { targetId = pasteEvent.targetId
                                        , text = pasteEvent.text
                                        }
                                , time = timestamp
                                }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                ResetBackend ->
                    eventsToEvent2 state startTime events2

                FromJsPort fromJsPort ->
                    eventsToEvent2
                        { previousEvent =
                            { eventType = FromJsPort2 clientId delay { port_ = fromJsPort.port_, data = fromJsPort.data }
                            , time = timestamp
                            }
                                |> Just
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                WindowResize resizeEvent ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = WindowResize2 clientId delay resizeEvent, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                CheckView checkView ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = CheckView2 clientId delay checkView, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseDown a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseDown2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseUp a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseUp2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseMove a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseMove2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseLeave a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseLeave2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseOver a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseOver2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseEnter a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseEnter2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                MouseOut a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = MouseOut2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                Focus a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = Focus2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                Blur a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = Blur2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2

                Wheel a ->
                    eventsToEvent2
                        { previousEvent = Just { eventType = Wheel2 clientId delay a, time = timestamp }
                        , rest = Maybe.Extra.toList previousEvent ++ rest
                        }
                        startTime
                        events2


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


codegen : ParsedCode -> Settings -> List Event -> String
codegen parsedCode settings events =
    let
        tests : List ( Event, List Event )
        tests =
            List.Extra.dropWhile (\a -> a.eventType /= ResetBackend) events
                |> List.Extra.groupWhile (\a _ -> a.eventType /= ResetBackend)
                |> List.filter (\( _, rest ) -> not (List.isEmpty rest))

        testsText : String
        testsText =
            tests
                |> List.indexedMap
                    (\index ( head, rest ) ->
                        testCode settings head.timestamp index (head :: rest)
                            |> Elm.Pretty.prettyExpression
                            |> Pretty.pretty 120
                            |> String.replace "\n" "\n    "
                    )
                |> String.join "\n    , "
                |> (\a ->
                        if parsedCode.noPriorTests || List.isEmpty tests then
                            a ++ "\n    "

                        else
                            "\n    , " ++ a
                   )
    in
    if settings.showAllCode then
        let
            httpRequests : String
            httpRequests =
                List.Extra.dropWhile (\a -> a.eventType /= ResetBackend) events
                    |> List.filterMap
                        (\event ->
                            case event.eventType of
                                Http http ->
                                    ( http.method ++ "_" ++ dropPrefix "http://localhost:8001/" http.url, http.filepath ) |> Just

                                _ ->
                                    Nothing
                        )
                    |> (\a -> parsedCode.httpRequests ++ a ++ localRequests)
                    |> Dict.fromList
                    |> Dict.toList
                    |> List.map (\( first, second ) -> "( \"" ++ first ++ "\", \"" ++ second ++ "\" )")
                    |> String.join "\n    , "
                    |> (\a -> "\nhttpRequests =\n    [ " ++ a ++ "\n    ]\n        ")

            localRequests : List ( String, String )
            localRequests =
                List.filterMap
                    (\event ->
                        case event.eventType of
                            HttpLocal { filepath } ->
                                Just ( "GET_" ++ filepath, "/public" ++ filepath )

                            _ ->
                                Nothing
                    )
                    events
        in
        List.map
            (\codePart ->
                case codePart of
                    UserCode code ->
                        code

                    HttpRequestCode ->
                        httpRequests

                    TestEntryPoint ->
                        testsText
            )
            parsedCode.codeParts
            |> String.concat

    else
        testsText


urlToStringNoDomain : String -> String
urlToStringNoDomain url =
    case String.split "/" url of
        "http:" :: _ :: _ :: rest ->
            "/" ++ String.join "/" rest

        "https:" :: _ :: _ :: rest ->
            "/" ++ String.join "/" rest

        _ ->
            url


eventToString : Int -> Settings -> List ClientId -> Int -> List EventType2 -> List Expression
eventToString depth settings clients startTime events =
    List.map
        (\event ->
            let
                client clientId =
                    case List.Extra.findIndex (\a -> a == clientId) clients of
                        Just index ->
                            "tab" ++ String.fromInt (index + 1)

                        Nothing ->
                            "tab"
            in
            case event of
                Connect2 clientId delay { url, sessionId, windowWidth, windowHeight } events2 ->
                    Codegen.apply
                        [ Codegen.fqFun [ "T" ] "connectFrontend"
                        , Codegen.int delay
                        , Codegen.apply
                            [ Codegen.fqFun [ "Effect", "Lamdera" ] "sessionIdFromString"
                            , Codegen.string sessionId
                            ]
                        , Codegen.string (urlToStringNoDomain url)
                        , Codegen.record
                            [ ( "width", Codegen.int windowWidth )
                            , ( "height", Codegen.int windowHeight )
                            ]
                        , Codegen.lambda
                            [ Codegen.varPattern (client clientId) ]
                            (Codegen.list
                                (eventToString (depth + 1) settings clients startTime events2)
                            )
                        ]

                WindowResize2 clientId delay resizeEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "resizeWindow"
                        , Codegen.int delay
                        , Codegen.record
                            [ ( "width", Codegen.int resizeEvent.width )
                            , ( "height", Codegen.int resizeEvent.height )
                            ]
                        ]

                KeyDown2 clientId delay keyEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "keyDown"
                        , Codegen.int delay
                        , targetIdFunc keyEvent.targetId
                        , Codegen.string keyEvent.key
                        , Codegen.list
                            (List.filterMap
                                (\( name, bool ) ->
                                    if bool then
                                        Just (Codegen.val name)

                                    else
                                        Nothing
                                )
                                [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                , ( "Key_AltHeld", keyEvent.altKey )
                                , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                , ( "Key_MetaHeld", keyEvent.metaKey )
                                ]
                            )
                        ]

                KeyUp2 clientId delay keyEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "keyUp"
                        , Codegen.int delay
                        , targetIdFunc keyEvent.targetId
                        , Codegen.string keyEvent.key
                        , Codegen.list
                            (List.filterMap
                                (\( name, bool ) ->
                                    if bool then
                                        Just (Codegen.val name)

                                    else
                                        Nothing
                                )
                                [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                , ( "Key_AltHeld", keyEvent.altKey )
                                , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                , ( "Key_MetaHeld", keyEvent.metaKey )
                                ]
                            )
                        ]

                Click2 clientId delay mouseEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "click"
                        , Codegen.int delay
                        , targetIdFunc mouseEvent.targetId
                        ]

                ClickLink2 clientId delay mouseEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "clickLink"
                        , Codegen.int delay
                        , Codegen.string mouseEvent.path
                        ]

                Input2 clientId delay { targetId, text } ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "input"
                        , Codegen.int delay
                        , targetIdFunc targetId
                        , Codegen.string text
                        ]

                FromJsPort2 clientId delay { port_, data } ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "portEvent"
                        , Codegen.int delay
                        , Codegen.string port_
                        , case Json.Decode.decodeString Json.Decode.float data of
                            Ok float ->
                                Codegen.apply [ Codegen.fqFun [ "Json", "Encode" ] "float", Codegen.float float ]

                            Err _ ->
                                case Json.Decode.decodeString Json.Decode.string data of
                                    Ok text ->
                                        Codegen.apply
                                            [ Codegen.fqFun [ "Json", "Encode" ] "string", Codegen.string text ]

                                    Err _ ->
                                        Codegen.apply
                                            [ Codegen.fqFun [ "Maybe" ] "withDefault"
                                            , Codegen.apply
                                                [ Codegen.fqFun [ "Json", "Encode" ] "object", Codegen.list [] ]
                                            , Codegen.apply
                                                [ Codegen.fqFun [ "Json", "Decode" ] "decodeValue"
                                                , Codegen.fqFun [ "Json", "Decode" ] "value"
                                                , Codegen.string data
                                                ]
                                            ]
                        ]

                PointerDown2 clientId delay a ->
                    pointerCodegen delay settings "pointerDown" (client clientId) a

                PointerUp2 clientId delay a ->
                    pointerCodegen delay settings "pointerUp" (client clientId) a

                PointerMove2 clientId delay a ->
                    pointerCodegen delay settings "pointerMove" (client clientId) a

                PointerLeave2 clientId delay a ->
                    pointerCodegen delay settings "pointerLeave" (client clientId) a

                PointerCancel2 clientId delay a ->
                    pointerCodegen delay settings "pointerCancel" (client clientId) a

                PointerOver2 clientId delay a ->
                    pointerCodegen delay settings "pointerOver" (client clientId) a

                PointerEnter2 clientId delay a ->
                    pointerCodegen delay settings "pointerEnter" (client clientId) a

                PointerOut2 clientId delay a ->
                    pointerCodegen delay settings "pointerOut" (client clientId) a

                TouchStart2 clientId delay a ->
                    touchCodegen delay "touchStart" (client clientId) a

                TouchCancel2 clientId delay a ->
                    touchCodegen delay "touchCancel" (client clientId) a

                TouchMove2 clientId delay a ->
                    touchCodegen delay "touchMove" (client clientId) a

                TouchEnd2 clientId delay a ->
                    touchCodegen delay "touchEnd" (client clientId) a

                CheckView2 clientId delay checkViewEvent ->
                    --client clientId
                    --    ++ ".checkView "
                    --    ++ String.fromInt delay
                    --    ++ " (Test.Html.Query.has [ "
                    --    ++ String.join ", " (List.map (\text -> "Selector.text \"" ++ text ++ "\"") checkViewEvent.selection)
                    --    ++ " ])"
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "checkView"
                        , Codegen.int delay
                        , Codegen.apply
                            [ Codegen.fqFun [ "Test", "Html", "Query" ] "has"
                            , Codegen.list
                                (List.map
                                    (\text ->
                                        Codegen.apply
                                            [ Codegen.fqFun [ "Test", "Html", "Selector" ] "text"
                                            , Codegen.string text
                                            ]
                                    )
                                    checkViewEvent.selection
                                )
                            ]
                        ]

                MouseDown2 clientId delay a ->
                    mouseCodegen delay settings "mouseDown" (client clientId) a

                MouseUp2 clientId delay a ->
                    mouseCodegen delay settings "mouseUp" (client clientId) a

                MouseMove2 clientId delay a ->
                    mouseCodegen delay settings "mouseMove" (client clientId) a

                MouseLeave2 clientId delay a ->
                    mouseCodegen delay settings "mouseLeave" (client clientId) a

                MouseOver2 clientId delay a ->
                    mouseCodegen delay settings "mouseOver" (client clientId) a

                MouseEnter2 clientId delay a ->
                    mouseCodegen delay settings "mouseEnter" (client clientId) a

                MouseOut2 clientId delay a ->
                    mouseCodegen delay settings "mouseOut" (client clientId) a

                Focus2 clientId delay a ->
                    --client clientId
                    --    ++ ".focus "
                    --    ++ String.fromInt delay
                    --    ++ " "
                    --    ++ targetIdFunc a.targetId
                    --    ++ "\n"
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "focus"
                        , Codegen.int delay
                        , targetIdFunc a.targetId
                        ]

                Blur2 clientId delay a ->
                    --client clientId
                    --    ++ ".blur "
                    --    ++ String.fromInt delay
                    --    ++ " "
                    --    ++ targetIdFunc a.targetId
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "blur"
                        , Codegen.int delay
                        , targetIdFunc a.targetId
                        ]

                Wheel2 clientId delay a ->
                    let
                        modifiers : List Expression
                        modifiers =
                            List.filterMap
                                (\( name, value, default ) ->
                                    if value == Codegen.string default then
                                        Nothing

                                    else
                                        Just (Codegen.apply [ Codegen.val name, value ])
                                )
                                [ ( "DeltaX", Codegen.float a.deltaX, "0" )
                                , ( "DeltaZ", Codegen.float a.deltaZ, "0" )
                                , ( "DeltaMode"
                                  , case a.deltaMode of
                                        1 ->
                                            Codegen.string "DeltaLine"

                                        2 ->
                                            Codegen.string "DeltaPage"

                                        _ ->
                                            Codegen.string "DeltaPixel"
                                  , "DeltaPixel"
                                  )
                                ]
                    in
                    --client clientId
                    --    ++ ".wheel "
                    --    ++ String.fromInt delay
                    --    ++ " "
                    --    ++ targetIdFunc a.mouseEvent.targetId
                    --    ++ " "
                    --    ++ String.fromFloat a.deltaY
                    --    ++ " ("
                    --    ++ String.fromFloat a.mouseEvent.offsetX
                    --    ++ ","
                    --    ++ String.fromFloat a.mouseEvent.offsetY
                    --    ++ ") [ "
                    --    ++ modifiers
                    --    ++ " ] "
                    --    ++ mouseEventModifiers settings a.mouseEvent
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "wheel"
                        , Codegen.int delay
                        , targetIdFunc a.mouseEvent.targetId
                        , Codegen.float a.deltaY
                        , Codegen.tuple
                            [ Codegen.float a.mouseEvent.offsetX
                            , Codegen.float a.mouseEvent.offsetY
                            ]
                        , Codegen.list modifiers
                        , mouseEventModifiers settings a.mouseEvent
                        ]
        )
        events


testCode : Settings -> Int -> Int -> List Event -> Expression
testCode settings startTime testIndex events =
    let
        clients : List ClientId
        clients =
            List.map .clientId events |> List.Extra.unique

        events2 =
            eventsToEvent2 { previousEvent = Nothing, rest = [] } startTime events
                |> eventToString 0 settings clients startTime
    in
    --" T.start\n        \"test"
    --    ++ String.fromInt testIndex
    --    ++ "\"\n        (Time.millisToPosix "
    --    ++ String.fromInt startTime
    --    ++ ")\n        config"
    --    ++ "\n        [ "
    --    ++ String.join "\n        , " events2
    --    ++ "\n        ]"
    Codegen.apply
        [ Codegen.fqFun [ "T" ] "start"
        , Codegen.string ("test" ++ String.fromInt testIndex)
        , Codegen.apply
            [ Codegen.fqFun [ "Time" ] "millisToPosix"
            , Codegen.int startTime
            ]
        , Codegen.fun "config"
        , Codegen.list events2
        ]


touchCodegen : MillisecondWaitBefore -> String -> String -> TouchEvent -> Expression
touchCodegen delay funcName client a =
    let
        touchToString : Touch -> Expression
        touchToString touch =
            Codegen.record
                [ ( "id", Codegen.int touch.identifier )
                , ( "screenPos", Codegen.tuple [ Codegen.float touch.screenX, Codegen.float touch.screenY ] )
                , ( "clientPos", Codegen.tuple [ Codegen.float touch.clientX, Codegen.float touch.clientY ] )
                , ( "pagePos", Codegen.tuple [ Codegen.float touch.pageX, Codegen.float touch.pageY ] )
                ]
    in
    Codegen.apply
        [ Codegen.access (Codegen.fun client) funcName
        , Codegen.int delay
        , targetIdFunc a.targetId
        , Codegen.record
            [ ( "targetTouches", Codegen.list (List.map touchToString a.targetTouches) )
            , ( "changedTouches", Codegen.list (List.map touchToString a.changedTouches) )
            ]
        ]


pointerCodegen : MillisecondWaitBefore -> Settings -> String -> String -> PointerEvent -> Expression
pointerCodegen delay { includeClientPos, includePagePos, includeScreenPos } funcName client a =
    let
        options : List Expression
        options =
            List.filterMap
                (\( name, include, ( x, y ) ) ->
                    if (x == a.offsetX && y == a.offsetY) || not include then
                        Nothing

                    else
                        Codegen.apply
                            [ Codegen.fun name
                            , Codegen.tuple [ Codegen.float x, Codegen.float y ]
                            ]
                            |> Just
                )
                [ ( "ScreenXY", includeScreenPos, ( a.screenX, a.screenY ) )
                , ( "PageXY", includePagePos, ( a.pageX, a.pageY ) )
                , ( "ClientXY", includeClientPos, ( a.clientX, a.clientY ) )
                ]
                ++ List.filterMap
                    identity
                    [ pointerButton a
                    , if a.altKey then
                        Just (Codegen.val "AltHeld")

                      else
                        Nothing
                    , if a.shiftKey then
                        Just (Codegen.val "ShiftHeld")

                      else
                        Nothing
                    , if a.ctrlKey then
                        Just (Codegen.val "CtrlHeld")

                      else
                        Nothing
                    , if a.metaKey then
                        Just (Codegen.val "MetaHeld")

                      else
                        Nothing
                    , if a.pointerId == 0 then
                        Nothing

                      else
                        Just (Codegen.apply [ Codegen.val "PointerId ", Codegen.int a.pointerId ])
                    , if a.isPrimary then
                        Nothing

                      else
                        Just (Codegen.val "IsNotPrimary")
                    ]
    in
    Codegen.apply
        [ Codegen.access (Codegen.fun client) funcName
        , Codegen.int delay
        , targetIdFunc a.targetId
        , Codegen.tuple [ Codegen.float a.offsetX, Codegen.float a.offsetY ]
        , Codegen.list options
        ]


mouseCodegen : MillisecondWaitBefore -> Settings -> String -> String -> MouseEvent -> Expression
mouseCodegen delay settings funcName client a =
    Codegen.apply
        [ Codegen.access (Codegen.fun client) funcName
        , Codegen.int delay
        , targetIdFunc a.targetId
        , Codegen.tuple [ Codegen.float a.offsetX, Codegen.float a.offsetY ]
        , mouseEventModifiers settings a
        ]


mouseEventModifiers : Settings -> MouseEvent -> Expression
mouseEventModifiers { includeClientPos, includePagePos, includeScreenPos } a =
    List.filterMap
        (\( name, include, ( x, y ) ) ->
            if (x == a.offsetX && y == a.offsetY) || not include then
                Nothing

            else
                Codegen.apply
                    [ Codegen.val name
                    , Codegen.float x
                    , Codegen.float y
                    ]
                    |> Just
        )
        [ ( "ScreenXY", includeScreenPos, ( a.screenX, a.screenY ) )
        , ( "PageXY", includePagePos, ( a.pageX, a.pageY ) )
        , ( "ClientXY", includeClientPos, ( a.clientX, a.clientY ) )
        ]
        ++ List.filterMap
            identity
            [ pointerButton a
            , if a.altKey then
                Just (Codegen.val "AltHeld")

              else
                Nothing
            , if a.shiftKey then
                Just (Codegen.val "ShiftHeld")

              else
                Nothing
            , if a.ctrlKey then
                Just (Codegen.val "CtrlHeld")

              else
                Nothing
            , if a.metaKey then
                Just (Codegen.val "MetaHeld")

              else
                Nothing
            ]
        |> Codegen.list


pointerButton : { a | button : Int } -> Maybe Codegen.Expression
pointerButton a =
    case a.button of
        1 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "MainButton" ])

        2 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "MiddleButton" ])

        3 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "SecondButton" ])

        4 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "BackButton" ])

        5 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "ForwardButton" ])

        _ ->
            Nothing


targetIdFunc : Maybe String -> Expression
targetIdFunc id =
    Codegen.apply [ Codegen.fqFun [ "Dom" ] "id", Codegen.string (Maybe.withDefault "id-attribute-missing" id) ]


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ case model of
            LoadingSession _ ->
                Html.text "Loading session..."

            LoadedSession loaded ->
                Ui.Anim.layout
                    { options = []
                    , breakpoints = Nothing
                    , toMsg = ElmUiMsg
                    }
                    loaded.elmUiState
                    [ Ui.Font.family [ Ui.Font.sansSerif ], Ui.height Ui.fill ]
                    (loadedView loaded)
        ]
    }


faintBorderColor =
    Ui.borderColor (Ui.rgb 160 150 140)


inlineCode : String -> Ui.Element msg
inlineCode text =
    Ui.el
        [ Ui.border 1
        , Ui.rounded 4
        , Ui.paddingXY 4 1
        , Ui.borderColor (Ui.rgb 160 160 160)
        , Ui.background (Ui.rgb 250 248 245)
        , Ui.Font.color (Ui.rgb 47 23 3)
        , Ui.Font.exactWhitespace
        ]
        (Ui.text text)


loadedView : LoadedData -> Ui.Element FrontendMsg
loadedView model =
    case model.parsedCode of
        WaitingOnUser ->
            Ui.column
                [ Ui.centerX, Ui.centerY, Ui.spacing 16, Ui.width Ui.shrink ]
                [ Ui.el
                    [ Ui.Input.button PressedNewFile
                    , Ui.border 1
                    , Ui.borderColor (Ui.rgb 100 100 100)
                    , Ui.background (Ui.rgb 240 240 240)
                    , Ui.padding 8
                    , Ui.Font.size 20
                    , Ui.rounded 8
                    , Ui.contentCenterX
                    ]
                    (Ui.text "New end-to-end test module")
                , Ui.el
                    [ Ui.Input.button PressedSelectFile
                    , Ui.border 1
                    , Ui.borderColor (Ui.rgb 100 100 100)
                    , Ui.background (Ui.rgb 240 240 240)
                    , Ui.padding 8
                    , Ui.Font.size 20
                    , Ui.rounded 8
                    , Ui.contentCenterX
                    ]
                    (Ui.text "Open existing end-to-end test module")
                ]

        ParseSuccess parsed ->
            let
                eventsList : List Event
                eventsList =
                    Array.toList model.history
            in
            Ui.row
                [ Ui.height Ui.fill ]
                [ Ui.column
                    [ Ui.height Ui.fill
                    , Ui.width Ui.shrink
                    , Ui.background (Ui.rgb 255 250 245)
                    , Ui.borderWith { left = 0, right = 1, top = 0, bottom = 0 }
                    , Ui.borderColor (Ui.rgb 120 110 100)
                    ]
                    [ Ui.row
                        [ Ui.borderWith { top = 0, left = 0, right = 0, bottom = 1 }, faintBorderColor ]
                        [ Ui.el [ Ui.Font.bold, Ui.paddingWith { left = 8, right = 8, top = 8, bottom = 4 } ] (Ui.text "Unsaved events")
                        , Ui.el
                            [ Ui.Input.button PressedResetSession
                            , Ui.border 1
                            , faintBorderColor
                            , Ui.background (Ui.rgb 240 235 230)
                            , Ui.width Ui.shrink
                            , Ui.paddingWith { left = 8, right = 8, top = 10, bottom = 6 }
                            , Ui.borderWith { top = 0, left = 1, right = 0, bottom = 0 }
                            ]
                            (Ui.text "Clear")
                        , Ui.el
                            [ Ui.Input.button PressedCommitToFile
                            , Ui.border 1
                            , faintBorderColor
                            , Ui.background (Ui.rgb 240 235 230)
                            , Ui.width Ui.shrink
                            , Ui.paddingWith { left = 8, right = 8, top = 10, bottom = 6 }
                            , Ui.borderWith { top = 0, left = 1, right = 0, bottom = 0 }
                            , case model.commitStatus of
                                CommitFailed ->
                                    Ui.Font.color (Ui.rgb 255 0 0)

                                _ ->
                                    Ui.noAttr
                            ]
                            (Ui.text
                                (case model.commitStatus of
                                    NotCommitted ->
                                        "Commit to file"

                                    CommitSuccess ->
                                        "Committed!"

                                    CommitFailed ->
                                        "Commit failed"

                                    Committing _ ->
                                        "Committing..."
                                )
                            )
                        ]
                    , eventsView eventsList
                    ]
                , if model.noEventsHaveArrived then
                    Ui.Prose.paragraph
                        [ Ui.widthMax 1000, Ui.padding 16, Ui.Font.size 20, Ui.htmlAttribute (Html.Attributes.style "white-space" "pre-wrap") ]
                        [ Ui.text "In order to record events from your app, "
                        , Ui.el
                            [ Ui.Input.button PressedDownloadTestGen
                            , Ui.Font.underline
                            , Ui.Font.color (Ui.rgb 10 80 255)
                            ]
                            (Ui.text "download this file")
                        , Ui.text " and place it in "
                        , inlineCode "<root folder>/elm-pkg-js/"
                        , Ui.text ".\n\nThen run "
                        , inlineCode "EXPERIMENTAL=1 lamdera live"
                        , Ui.text " (Mac/Linux terminal) or "
                        , inlineCode "set \"EXPERIMENTAL=1\" & lamdera live"
                        , Ui.text " (Windows cmd) for it to take effect.\n\nIt's recommended that you add "
                        , inlineCode "/elm-pkg-js/test-gen.js"
                        , Ui.text " to your "
                        , inlineCode ".gitignore"
                        , Ui.text " file to avoid accidentally deploying this code to production."
                        ]

                  else
                    Ui.column
                        [ Ui.height Ui.fill, Ui.spacing 0 ]
                        ([ Ui.column
                            [ Ui.spacing 8, Ui.padding 8 ]
                            [ Ui.Prose.paragraph
                                [ Ui.Font.size 16, Ui.width Ui.shrink, Ui.spacing 4 ]
                                [ Ui.text "Press "
                                , Ui.el
                                    [ Ui.Font.color (Ui.rgb 238 238 238)
                                    , Ui.background (Ui.rgb 41 51 53)
                                    , Ui.Font.size 14
                                    , Ui.contentCenterY
                                    , Ui.padding 4
                                    ]
                                    (Ui.text "Reset\u{00A0}Backend")
                                , Ui.text " in lamdera live to start a new test"
                                ]
                            , Ui.column
                                []
                                [ simpleCheckbox ToggledIncludeClientPos "Include clientPos in pointer events" model.settings.includeClientPos
                                , simpleCheckbox ToggledIncludePagePos "Include pagePos in pointer events" model.settings.includePagePos
                                , simpleCheckbox ToggledIncludeScreenPos "Include screenPos in pointer events" model.settings.includeScreenPos
                                , simpleCheckbox ToggledShowAllCode "Show all generated code" model.settings.showAllCode
                                ]
                            ]
                         , codegen parsed model.settings (List.filter (\event -> not event.isHidden) eventsList)
                            |> Ui.text
                            |> Ui.el
                                [ Ui.Font.family [ Ui.Font.monospace ]
                                , Ui.Font.size 12
                                , Ui.Font.exactWhitespace
                                , Ui.scrollable
                                , Ui.padding 8
                                , Ui.borderWith { top = 1, left = 0, right = 0, bottom = 0 }
                                , faintBorderColor
                                ]
                         ]
                         --(case model.parsedCode of
                         --    ParseSuccess _ ->
                         --        [ Ui.el
                         --            [ Ui.borderWith { left = 0, top = 1, bottom = 0, right = 0 }
                         --            , Ui.borderColor (Ui.rgb 100 100 100)
                         --            , Ui.inFront
                         --                (Ui.row
                         --                    [ Ui.Input.button PressedCopyCode
                         --                    , Ui.border 1
                         --                    , Ui.borderColor (Ui.rgb 100 100 100)
                         --                    , Ui.background (Ui.rgb 240 240 240)
                         --                    , Ui.width Ui.shrink
                         --                    , Ui.padding 4
                         --                    , Ui.roundedWith { topLeft = 0, topRight = 0, bottomRight = 0, bottomLeft = 4 }
                         --                    , Ui.alignRight
                         --                    , Ui.move { x = 0, y = -1, z = 0 }
                         --                    , Ui.Font.size 14
                         --                    , Ui.spacing 4
                         --                    ]
                         --                    [ Icons.copy
                         --                    , if model.copyCounter > 0 then
                         --                        Ui.text "Copied!"
                         --
                         --                      else
                         --                        Ui.text "Copy to clipboard"
                         --                    ]
                         --                )
                         --            ]
                         --            Ui.none
                         --        ]
                         --
                         --    _ ->
                         --        []
                         --)
                        )
                ]

        ParseFailed error ->
            parseErrorToString error |> Ui.text |> Ui.el []

        FileApiNotSupported ->
            Ui.el
                [ Ui.centerX, Ui.centerY, Ui.widthMax 600, Ui.Font.size 20 ]
                (Ui.text "Your browser doesn't support the File System API. It's needed in order to read and write to your end-to-end test module. It should work if you switch to Chrome (sorry).")


parseErrorToString : ParseError -> String
parseErrorToString error =
    case error of
        InvalidHttpRequests ->
            "The httpRequests function was found but couldn't be parsed."

        HttpRequestsNotFound ->
            "The httpRequests function wasn't found."

        TestEntryPointNotFound ->
            "The test entry point (the \"]\" at the end of the the tests function) wasn't found."

        UnknownError ->
            "This end-to-end test module appears to be corrupted or the wrong file is being loaded."

        HttpRequestsEndNotFound ->
            "The httpRequests function was found but it's supposed to end with \"|> Dict.fromList\""


simpleCheckbox : (Bool -> msg) -> String -> Bool -> Ui.Element msg
simpleCheckbox msg text value =
    let
        { element, id } =
            Ui.Input.label text [ Ui.Font.size 14, Ui.move { x = 0, y = 2, z = 0 } ] (Ui.text text)
    in
    Ui.row
        [ Ui.spacing 4 ]
        [ Ui.Input.checkbox
            []
            { onChange = msg
            , icon = Nothing
            , checked = value
            , label = id
            }
        , element
        ]


eventsView : List Event -> Ui.Element FrontendMsg
eventsView events =
    case events of
        [] ->
            Ui.el [ Ui.padding 16, Ui.width Ui.shrink ] (Ui.text "No events have arrived")

        _ ->
            List.indexedMap
                (\index event ->
                    let
                        text : String
                        text =
                            case event.eventType of
                                KeyDown keyEvent ->
                                    keyEvent.key ++ " key down"

                                KeyUp keyEvent ->
                                    keyEvent.key ++ " key up"

                                Click mouseEvent ->
                                    case mouseEvent.targetId of
                                        Just id ->
                                            "Click " ++ id

                                        Nothing ->
                                            "Click nothing"

                                Http _ ->
                                    "Http request"

                                Connect record ->
                                    "Connected " ++ record.sessionId

                                ClickLink linkEvent ->
                                    "Click link " ++ linkEvent.path

                                Paste pasteEvent ->
                                    "Pasted text " ++ pasteEvent.text

                                Input inputEvent ->
                                    "Text input " ++ inputEvent.text

                                ResetBackend ->
                                    "Test ended"

                                FromJsPort fromJsPortEvent ->
                                    "Port " ++ fromJsPortEvent.port_ ++ " " ++ fromJsPortEvent.data

                                HttpLocal { filepath } ->
                                    "Loaded " ++ filepath

                                WindowResize { width, height } ->
                                    "Window resized w:" ++ String.fromInt width ++ " h:" ++ String.fromInt height

                                PointerDown _ ->
                                    "Pointer down"

                                PointerUp _ ->
                                    "Pointer up"

                                PointerMove _ ->
                                    "Pointer move"

                                PointerLeave _ ->
                                    "Pointer leave"

                                PointerCancel _ ->
                                    "Pointer cancel"

                                PointerOver _ ->
                                    "Pointer over"

                                PointerEnter _ ->
                                    "Pointer enter"

                                PointerOut _ ->
                                    "Pointer out"

                                TouchStart _ ->
                                    "Touch start"

                                TouchCancel _ ->
                                    "Touch cancel"

                                TouchMove _ ->
                                    "Touch move"

                                TouchEnd _ ->
                                    "Touch end"

                                CheckView _ ->
                                    "Check view"

                                MouseDown _ ->
                                    "Mouse down"

                                MouseUp _ ->
                                    "Mouse up"

                                MouseMove _ ->
                                    "Mouse move"

                                MouseLeave _ ->
                                    "Mouse leave"

                                MouseOver _ ->
                                    "Mouse over"

                                MouseEnter _ ->
                                    "Mouse enter"

                                MouseOut _ ->
                                    "Mouse out"

                                Focus _ ->
                                    "Focus"

                                Blur _ ->
                                    "Blur"

                                Wheel _ ->
                                    "Mouse wheel scrolled"
                    in
                    Ui.row
                        [ Ui.Input.button PressedEvent
                        , Ui.Events.onMouseDown (MouseDownOnEvent index (not event.isHidden))
                        , Ui.Events.onMouseEnter (MouseEnterOnEvent index (not event.isHidden))
                        , Ui.spacing 4
                        , Ui.Font.color
                            (if event.isHidden then
                                Ui.rgb 120 120 120

                             else
                                Ui.rgb 0 0 0
                            )
                        , Ui.htmlAttribute (Html.Attributes.title text)
                        , Ui.paddingXY 4 0
                        ]
                        [ if event.isHidden then
                            Icons.eyeClosed

                          else
                            Icons.eye
                        , Ui.text text
                        ]
                )
                events
                |> Ui.column
                    [ Ui.Font.size 14
                    , Ui.paddingXY 8 0
                    , Ui.paddingWith { left = 0, right = 0, top = 4, bottom = 8 }
                    , Ui.clipWithEllipsis
                    , Ui.width (Ui.px 320)
                    ]
                |> Ui.el [ Ui.id eventsListContainer, Ui.scrollable, Ui.height Ui.fill, Ui.width Ui.shrink ]
