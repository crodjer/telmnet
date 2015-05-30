module Telmnet where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JsonD
import Regex exposing (regex, contains)

-- MODEL
type alias Message =
  {
    source: String,
    text: String
  }

type alias Model =
  {
    server: String
  , log: List Message
  , connected: Bool
  , serverInput: String
  , promptInput: String
  }

incoming : String
incoming = "in"

outgoing : String
outgoing = "out"

init : Model
init =
  {
    server = "" -- "ws://localhost:9000"
  , log = []
  , connected = True
  , serverInput = "ws://localhost:9000"
  , promptInput = ""
  }


-- ACTIONS
type Action = Connect Bool
            | UpdateServer String
            | UpdatePrompt String
            | Send String
            | Receive String
            | Refocus String
            | NoOp

update : Action -> Model -> Model
update act model =
  let mkMessage src msg =
        { source = src
        , text = msg
        }
  in
  case act of
    UpdateServer string -> { model | serverInput <- string }
    Connect value       -> { model |
                             connected <- value
                           , log <- []
                           , server <- model.serverInput
                           , serverInput <- ""
                           }

    UpdatePrompt string -> { model | promptInput <- string }
    Send message        -> { model |
                             log         <- model.log ++ [mkMessage outgoing message]
                           , promptInput <- ""
                           }
    Receive message     -> { model |
                             log <- model.log ++ [mkMessage incoming message]
                           }
    _                   -> model

-- VIEW

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  let is13 code = if code == 13 then Ok () else Err "not the right key code"
  in
    on "keydown"
         (JsonD.customDecoder keyCode is13)
         (\_ -> Signal.message address value)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ id "container" ]
      [ headerView address model
      , terminalView address model
      ]

headerView : Signal.Address Action -> Model -> Html
headerView address model =
  div []
      [ input [ id "server"
              , classList [ ("hidden", model.connected) ]
              , autofocus (not model.connected)
              , value model.serverInput
              , on "input" targetValue (UpdateServer >> Signal.message address)
              , onEnter address (Connect True)  ]
        [ ]
      , div [ id "toggle-connect"
            , classList [ ("hidden", not model.connected), ("inline-block", True) ]
            ]
        [ text <| "Connected to: " ++ model.server ]
      , button [ id "toggle-connect"
               , onClick address (Connect <| not model.connected)  ]
        [ text <| if model.connected then "Disconnect" else "Connect" ]
      ]

terminalView : Signal.Address Action -> Model -> Html
terminalView address model =
  div [ id "terminal"
      , classList [ ("hidden", not model.connected) ]
      , onClick address (Refocus "prompt")
      , onBlur address (Refocus "prompt")
      ]
        ((List.map (logView address) model.log)
         ++ (div [ class "clearfix" ] []) :: [promptView address model])

logView address message =
    div [ classList [ ("log-entry", True)
                    , ("log-entry-" ++ message.source, True)
                    , ("float-left", not <| contains (regex "[\n|\r]$") message.text) ]]
        [ text message.text ]

promptView : Signal.Address Action -> Model -> Html
promptView address model =
  input [ id "prompt"
        , autofocus model.connected
        , value model.promptInput
        , on "input" targetValue (UpdatePrompt >> Signal.message address)
        , onEnter address
                    (Send (model.promptInput ++ "\n"))
        ]
        []

main : Signal Html
main =
  Signal.map (view actions.address) model

model : Signal Model
model =
  Signal.foldp update init signals

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

signals : Signal Action
signals = Signal.merge (Signal.map Receive receiveMessage) actions.signal

port reFocus : Signal String
port reFocus =
  let prompt = Just "prompt"
      server = Just "server"
      focusId act =
        case act of
          Connect True   ->  prompt
          Send _         ->  prompt
          Receive _      ->  prompt
          UpdateServer _ ->  Nothing
          UpdatePrompt _ ->  Nothing
          Refocus elId    -> Just elId
          _              ->  server
  in
    Signal.filterMap focusId "" signals

port connectStatus : Signal Bool
port connectStatus = Signal.map .connected model

port receiveMessage : Signal String

port sendMessage : Signal String
port sendMessage =
  let getMessage act =
        case act of
          Send message -> Just message
          _            -> Nothing
  in
    Signal.filterMap getMessage "" actions.signal
