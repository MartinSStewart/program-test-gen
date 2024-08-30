module Types exposing (..)

import Array exposing (Array)
import AssocList
import AssocSet
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Ui.Anim
import Url exposing (Url)


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias LoadingData =
    { key : Key, sessionName : SessionName }


type alias LoadedData =
    { key : Key
    , sessionName : SessionName
    , history : Array Event
    , copyCounter : Int
    , elmUiState : Ui.Anim.State
    , includeClientPos : Bool
    , includePagePos : Bool
    , includeScreenPos : Bool
    }


type alias BackendModel =
    { sessions : AssocList.Dict SessionName Session
    }


type alias Session =
    { history : Array Event
    , connections : AssocSet.Set ClientId
    , includeClientPos : Bool
    , includePagePos : Bool
    , includeScreenPos : Bool
    }


type SessionName
    = SessionName String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedResetSession
    | GotRandomSessionName SessionName
    | ScrolledToBottom
    | PressedSetEventVisibility Int Bool
    | PressedCopyCode
    | ElmUiMsg Ui.Anim.Msg
    | ToggledIncludeScreenPos Bool
    | ToggledIncludeClientPos Bool
    | ToggledIncludePagePos Bool


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest
    | SetEventVisibilityRequest { index : Int, isHidden : Bool }
    | SetIncludeScreenPageClientPos { includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool }


type BackendMsg
    = ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse { history : Array Event, includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool }
    | SessionUpdate Event
    | ResetSession


type alias Event =
    { isHidden : Bool
    , timestamp : Int
    , eventType : EventType
    , clientId : ClientId
    }


type EventType
    = KeyDown KeyEvent
    | KeyUp KeyEvent
    | Click MouseEvent
    | ClickLink LinkEvent
    | Http HttpEvent
    | HttpLocal HttpLocalEvent
    | Connect ConnectEvent
    | Paste PasteEvent
    | Input InputEvent
    | ResetBackend
    | FromJsPort FromJsPortEvent
    | WindowResize WindowResizeEvent
    | PointerDown PointerEvent
    | PointerUp PointerEvent
    | PointerMove PointerEvent
    | PointerLeave PointerEvent
    | PointerCancel PointerEvent
    | PointerOver PointerEvent
    | PointerEnter PointerEvent
    | PointerOut PointerEvent
    | TouchStart TouchEvent
    | TouchCancel TouchEvent
    | TouchMove TouchEvent
    | TouchEnd TouchEvent
    | CheckView CheckViewEvent


type alias CheckViewEvent =
    { selection : List String
    }


type alias PointerEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    , pointerType : String
    , pointerId : Int
    , isPrimary : Bool
    , width : Float
    , height : Float
    , pressure : Float
    , tiltX : Float
    , tiltY : Float
    }


type alias TouchEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , changedTouches : List Touch
    , targetTouches : List Touch
    , touches : List Touch
    }


type alias Touch =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , identifier : Int
    }


type alias ConnectEvent =
    { url : String, sessionId : SessionId, windowWidth : Int, windowHeight : Int, code : Maybe String }


type alias WindowResizeEvent =
    { width : Int, height : Int }


type alias HttpLocalEvent =
    { filepath : String }


type alias FromJsPortEvent =
    { triggeredFromPort : Maybe String, port_ : String, data : String }


type alias InputEvent =
    { targetId : String, text : String }


type alias KeyEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , key : String
    }


type alias MouseEvent =
    { targetId : Maybe String
    }


type alias PasteEvent =
    { targetId : Maybe String
    , text : String
    }


type alias LinkEvent =
    { path : String
    }


type alias HttpEvent =
    { responseType : String
    , method : String
    , url : String
    , filepath : String
    }
