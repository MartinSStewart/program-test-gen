module Evergreen.Migrate.V4 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Array
import AssocList
import Evergreen.V2.Internal.Model2
import Evergreen.V2.SessionName
import Evergreen.V2.Types
import Evergreen.V2.Ui.Anim
import Evergreen.V4.Internal.Model2
import Evergreen.V4.SessionName
import Evergreen.V4.Types
import Evergreen.V4.Ui.Anim
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V2.Types.FrontendModel -> ModelMigration Evergreen.V4.Types.FrontendModel Evergreen.V4.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V2.Types.BackendModel -> ModelMigration Evergreen.V4.Types.BackendModel Evergreen.V4.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V2.Types.FrontendMsg -> MsgMigration Evergreen.V4.Types.FrontendMsg Evergreen.V4.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V2.Types.ToBackend -> MsgMigration Evergreen.V4.Types.ToBackend Evergreen.V4.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V2.Types.BackendMsg -> MsgMigration Evergreen.V4.Types.BackendMsg Evergreen.V4.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V2.Types.ToFrontend -> MsgMigration Evergreen.V4.Types.ToFrontend Evergreen.V4.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_BackendModel : Evergreen.V2.Types.BackendModel -> Evergreen.V4.Types.BackendModel
migrate_Types_BackendModel old =
    { sessions = old.sessions |> migrate_AssocList_Dict migrate_SessionName_SessionName migrate_Types_Session
    }


migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
migrate_AssocList_Dict migrate_a migrate_b old =
    old
        |> AssocList.toList
        |> List.map (Tuple.mapBoth migrate_a migrate_b)
        |> AssocList.fromList


migrate_Internal_Model2_State : Evergreen.V2.Internal.Model2.State -> Evergreen.V4.Internal.Model2.State
migrate_Internal_Model2_State old =
    case old of
        Evergreen.V2.Internal.Model2.State p0 ->
            Evergreen.V4.Internal.Model2.State p0


migrate_SessionName_SessionName : Evergreen.V2.SessionName.SessionName -> Evergreen.V4.SessionName.SessionName
migrate_SessionName_SessionName old =
    case old of
        Evergreen.V2.SessionName.SessionName p0 ->
            Evergreen.V4.SessionName.SessionName p0


migrate_Types_BlurEvent : Evergreen.V2.Types.BlurEvent -> Evergreen.V4.Types.BlurEvent
migrate_Types_BlurEvent old =
    old


migrate_Types_CheckViewEvent : Evergreen.V2.Types.CheckViewEvent -> Evergreen.V4.Types.CheckViewEvent
migrate_Types_CheckViewEvent old =
    old


migrate_Types_ClickEvent : Evergreen.V2.Types.ClickEvent -> Evergreen.V4.Types.ClickEvent
migrate_Types_ClickEvent old =
    old


migrate_Types_Code : Evergreen.V2.Types.Code -> Evergreen.V4.Types.Code
migrate_Types_Code old =
    case old of
        Evergreen.V2.Types.UserCode p0 ->
            Evergreen.V4.Types.UserCode p0

        Evergreen.V2.Types.HttpRequestCode ->
            Evergreen.V4.Types.HttpRequestCode

        Evergreen.V2.Types.PortRequestCode ->
            Evergreen.V4.Types.PortRequestCode

        Evergreen.V2.Types.TestEntryPoint ->
            Evergreen.V4.Types.TestEntryPoint


migrate_Types_CommitStatus : Evergreen.V2.Types.CommitStatus -> Evergreen.V4.Types.CommitStatus
migrate_Types_CommitStatus old =
    case old of
        Evergreen.V2.Types.NotCommitted ->
            Evergreen.V4.Types.NotCommitted

        Evergreen.V2.Types.Committing p0 ->
            Evergreen.V4.Types.Committing p0

        Evergreen.V2.Types.CommitSuccess ->
            Evergreen.V4.Types.CommitSuccess

        Evergreen.V2.Types.CommitFailed ->
            Evergreen.V4.Types.CommitFailed


migrate_Types_ConnectEvent : Evergreen.V2.Types.ConnectEvent -> Evergreen.V4.Types.ConnectEvent
migrate_Types_ConnectEvent old =
    old


migrate_Types_Event : Evergreen.V2.Types.Event -> Evergreen.V4.Types.Event
migrate_Types_Event old =
    { isHidden = old.isHidden
    , timestamp = old.timestamp
    , eventType = old.eventType |> migrate_Types_EventType
    , clientId = old.clientId
    }


migrate_Types_EventType : Evergreen.V2.Types.EventType -> Evergreen.V4.Types.EventType
migrate_Types_EventType old =
    case old of
        Evergreen.V2.Types.KeyDown p0 ->
            Evergreen.V4.Types.KeyDown (p0 |> migrate_Types_KeyEvent)

        Evergreen.V2.Types.KeyUp p0 ->
            Evergreen.V4.Types.KeyUp (p0 |> migrate_Types_KeyEvent)

        Evergreen.V2.Types.Click p0 ->
            Evergreen.V4.Types.Click (p0 |> migrate_Types_ClickEvent)

        Evergreen.V2.Types.ClickLink p0 ->
            Evergreen.V4.Types.ClickLink (p0 |> migrate_Types_LinkEvent)

        Evergreen.V2.Types.Http p0 ->
            Evergreen.V4.Types.Http (p0 |> migrate_Types_HttpEvent)

        Evergreen.V2.Types.HttpLocal p0 ->
            Evergreen.V4.Types.HttpLocal (p0 |> migrate_Types_HttpLocalEvent)

        Evergreen.V2.Types.Connect p0 ->
            Evergreen.V4.Types.Connect (p0 |> migrate_Types_ConnectEvent)

        Evergreen.V2.Types.Paste p0 ->
            Evergreen.V4.Types.Paste (p0 |> migrate_Types_PasteEvent)

        Evergreen.V2.Types.Input p0 ->
            Evergreen.V4.Types.Input (p0 |> migrate_Types_InputEvent)

        Evergreen.V2.Types.ResetBackend ->
            Evergreen.V4.Types.ResetBackend

        Evergreen.V2.Types.FromJsPort p0 ->
            Evergreen.V4.Types.FromJsPort (p0 |> migrate_Types_FromJsPortEvent)

        Evergreen.V2.Types.WindowResize p0 ->
            Evergreen.V4.Types.WindowResize (p0 |> migrate_Types_WindowResizeEvent)

        Evergreen.V2.Types.PointerDown p0 ->
            Evergreen.V4.Types.PointerDown (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerUp p0 ->
            Evergreen.V4.Types.PointerUp (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerMove p0 ->
            Evergreen.V4.Types.PointerMove (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerLeave p0 ->
            Evergreen.V4.Types.PointerLeave (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerCancel p0 ->
            Evergreen.V4.Types.PointerCancel (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerOver p0 ->
            Evergreen.V4.Types.PointerOver (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerEnter p0 ->
            Evergreen.V4.Types.PointerEnter (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.PointerOut p0 ->
            Evergreen.V4.Types.PointerOut (p0 |> migrate_Types_PointerEvent)

        Evergreen.V2.Types.TouchStart p0 ->
            Evergreen.V4.Types.TouchStart (p0 |> migrate_Types_TouchEvent)

        Evergreen.V2.Types.TouchCancel p0 ->
            Evergreen.V4.Types.TouchCancel (p0 |> migrate_Types_TouchEvent)

        Evergreen.V2.Types.TouchMove p0 ->
            Evergreen.V4.Types.TouchMove (p0 |> migrate_Types_TouchEvent)

        Evergreen.V2.Types.TouchEnd p0 ->
            Evergreen.V4.Types.TouchEnd (p0 |> migrate_Types_TouchEvent)

        Evergreen.V2.Types.CheckView p0 ->
            Evergreen.V4.Types.CheckView (p0 |> migrate_Types_CheckViewEvent)

        Evergreen.V2.Types.MouseDown p0 ->
            Evergreen.V4.Types.MouseDown (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.MouseUp p0 ->
            Evergreen.V4.Types.MouseUp (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.MouseMove p0 ->
            Evergreen.V4.Types.MouseMove (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.MouseLeave p0 ->
            Evergreen.V4.Types.MouseLeave (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.MouseOver p0 ->
            Evergreen.V4.Types.MouseOver (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.MouseEnter p0 ->
            Evergreen.V4.Types.MouseEnter (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.MouseOut p0 ->
            Evergreen.V4.Types.MouseOut (p0 |> migrate_Types_MouseEvent)

        Evergreen.V2.Types.Focus p0 ->
            Evergreen.V4.Types.Focus (p0 |> migrate_Types_FocusEvent)

        Evergreen.V2.Types.Blur p0 ->
            Evergreen.V4.Types.Blur (p0 |> migrate_Types_BlurEvent)


migrate_Types_FocusEvent : Evergreen.V2.Types.FocusEvent -> Evergreen.V4.Types.FocusEvent
migrate_Types_FocusEvent old =
    old


migrate_Types_FromJsPortEvent : Evergreen.V2.Types.FromJsPortEvent -> Evergreen.V4.Types.FromJsPortEvent
migrate_Types_FromJsPortEvent old =
    old


migrate_Types_FrontendModel : Evergreen.V2.Types.FrontendModel -> Evergreen.V4.Types.FrontendModel
migrate_Types_FrontendModel old =
    case old of
        Evergreen.V2.Types.LoadingSession p0 ->
            Evergreen.V4.Types.LoadingSession (p0 |> migrate_Types_LoadingData)

        Evergreen.V2.Types.LoadedSession p0 ->
            Evergreen.V4.Types.LoadedSession (p0 |> migrate_Types_LoadedData)


migrate_Types_HttpEvent : Evergreen.V2.Types.HttpEvent -> Evergreen.V4.Types.HttpEvent
migrate_Types_HttpEvent old =
    old


migrate_Types_HttpLocalEvent : Evergreen.V2.Types.HttpLocalEvent -> Evergreen.V4.Types.HttpLocalEvent
migrate_Types_HttpLocalEvent old =
    old


migrate_Types_InputEvent : Evergreen.V2.Types.InputEvent -> Evergreen.V4.Types.InputEvent
migrate_Types_InputEvent old =
    old


migrate_Types_KeyEvent : Evergreen.V2.Types.KeyEvent -> Evergreen.V4.Types.KeyEvent
migrate_Types_KeyEvent old =
    old


migrate_Types_LinkEvent : Evergreen.V2.Types.LinkEvent -> Evergreen.V4.Types.LinkEvent
migrate_Types_LinkEvent old =
    old


migrate_Types_LoadedData : Evergreen.V2.Types.LoadedData -> Evergreen.V4.Types.LoadedData
migrate_Types_LoadedData old =
    { key = old.key
    , sessionName = old.sessionName |> migrate_SessionName_SessionName
    , history = old.history |> Array.map migrate_Types_Event
    , copyCounter = old.copyCounter
    , elmUiState = old.elmUiState |> migrate_Ui_Anim_State
    , settings = old.settings |> migrate_Types_Settings
    , parsedCode = old.parsedCode |> migrate_Types_ParsedCodeStatus
    , mouseDownOnEvent = old.mouseDownOnEvent
    , commitStatus = old.commitStatus |> migrate_Types_CommitStatus
    , noEventsHaveArrived = old.noEventsHaveArrived
    }


migrate_Types_LoadingData : Evergreen.V2.Types.LoadingData -> Evergreen.V4.Types.LoadingData
migrate_Types_LoadingData old =
    { key = old.key
    , sessionName = old.sessionName |> migrate_SessionName_SessionName
    }


migrate_Types_MouseEvent : Evergreen.V2.Types.MouseEvent -> Evergreen.V4.Types.MouseEvent
migrate_Types_MouseEvent old =
    old


migrate_Types_ParseError : Evergreen.V2.Types.ParseError -> Evergreen.V4.Types.ParseError
migrate_Types_ParseError old =
    case old of
        Evergreen.V2.Types.InvalidPortRequests ->
            Evergreen.V4.Types.InvalidPortRequests

        Evergreen.V2.Types.InvalidHttpRequests ->
            Evergreen.V4.Types.InvalidHttpRequests

        Evergreen.V2.Types.InvalidHttpAndPortRequests ->
            Evergreen.V4.Types.InvalidHttpAndPortRequests

        Evergreen.V2.Types.PortRequestsNotFound ->
            Evergreen.V4.Types.PortRequestsNotFound

        Evergreen.V2.Types.HttpRequestsNotFound ->
            Evergreen.V4.Types.HttpRequestsNotFound

        Evergreen.V2.Types.TestEntryPointNotFound ->
            Evergreen.V4.Types.TestEntryPointNotFound

        Evergreen.V2.Types.PortRequestsEndNotFound ->
            Evergreen.V4.Types.PortRequestsEndNotFound

        Evergreen.V2.Types.HttpRequestsEndNotFound ->
            Evergreen.V4.Types.HttpRequestsEndNotFound

        Evergreen.V2.Types.UnknownError ->
            Evergreen.V4.Types.UnknownError


migrate_Types_ParsedCode : Evergreen.V2.Types.ParsedCode -> Evergreen.V4.Types.ParsedCode
migrate_Types_ParsedCode old =
    { codeParts = old.codeParts |> List.map migrate_Types_Code
    , httpRequests = old.httpRequests
    , portRequests = old.portRequests
    , noPriorTests = old.noPriorTests
    }


migrate_Types_ParsedCodeStatus : Evergreen.V2.Types.ParsedCodeStatus -> Evergreen.V4.Types.ParsedCodeStatus
migrate_Types_ParsedCodeStatus old =
    case old of
        Evergreen.V2.Types.ParseSuccess p0 ->
            Evergreen.V4.Types.ParseSuccess (p0 |> migrate_Types_ParsedCode)

        Evergreen.V2.Types.ParseFailed p0 ->
            Evergreen.V4.Types.ParseFailed (p0 |> migrate_Types_ParseError)

        Evergreen.V2.Types.WaitingOnUser ->
            Evergreen.V4.Types.WaitingOnUser

        Evergreen.V2.Types.FileApiNotSupported ->
            Evergreen.V4.Types.FileApiNotSupported


migrate_Types_PasteEvent : Evergreen.V2.Types.PasteEvent -> Evergreen.V4.Types.PasteEvent
migrate_Types_PasteEvent old =
    old


migrate_Types_PointerEvent : Evergreen.V2.Types.PointerEvent -> Evergreen.V4.Types.PointerEvent
migrate_Types_PointerEvent old =
    old


migrate_Types_Session : Evergreen.V2.Types.Session -> Evergreen.V4.Types.Session
migrate_Types_Session old =
    { history = old.history |> Array.map migrate_Types_Event
    , connections = old.connections
    , settings = old.settings |> migrate_Types_Settings
    }


migrate_Types_Settings : Evergreen.V2.Types.Settings -> Evergreen.V4.Types.Settings
migrate_Types_Settings old =
    old


migrate_Types_ToFrontend : Evergreen.V2.Types.ToFrontend -> Evergreen.V4.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V2.Types.LoadSessionResponse p0 ->
            Evergreen.V4.Types.LoadSessionResponse
                { history = p0.history |> Array.map migrate_Types_Event
                , settings = p0.settings |> migrate_Types_Settings
                }

        Evergreen.V2.Types.SessionUpdate p0 ->
            Evergreen.V4.Types.SessionUpdate (p0 |> migrate_Types_Event)

        Evergreen.V2.Types.ResetSession ->
            Evergreen.V4.Types.ResetSession


migrate_Types_TouchEvent : Evergreen.V2.Types.TouchEvent -> Evergreen.V4.Types.TouchEvent
migrate_Types_TouchEvent old =
    old


migrate_Types_WindowResizeEvent : Evergreen.V2.Types.WindowResizeEvent -> Evergreen.V4.Types.WindowResizeEvent
migrate_Types_WindowResizeEvent old =
    old


migrate_Ui_Anim_State : Evergreen.V2.Ui.Anim.State -> Evergreen.V4.Ui.Anim.State
migrate_Ui_Anim_State old =
    old |> migrate_Internal_Model2_State
