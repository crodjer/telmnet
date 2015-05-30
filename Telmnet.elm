module Telmnet where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JsonD
import Regex exposing (regex, contains, split, replace)

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
  , connectionError: Maybe String
  }

newline : Regex.Regex
newline = regex "[\n|\r]"

incoming : String
incoming = "in"

outgoing : String
outgoing = "out"

protocol : String
protocol = "ws://"

init : Model
init =
  {
    server = ""
  , log = []
  , connected = False
  , serverInput = protocol ++ "localhost:9000"
  , promptInput = ""
  , connectionError = Nothing
  }


-- ACTIONS
type Action = Connect String
            | Disconnect (Maybe String)
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
    Connect server      -> { model |
                             connected       <- True
                           , log             <- []
                           , server          <- server
                           , connectionError <- Nothing
                           }
    Disconnect err      -> { model |
                             connected       <- False
                           , server          <- ""
                           , connectionError <- err
                           }

    UpdatePrompt string -> { model | promptInput <- string }
    Send message        -> { model |
                             log         <- model.log ++ [mkMessage outgoing message]
                           , promptInput <- ""
                           }
    Receive message     -> { model |
                             log <- model.log
                                    ++ (message |> split Regex.All newline
                                                |> List.map (\s -> s ++ "\n")
                                                |> List.map (mkMessage incoming))
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
  let hadConnectionError =
        case model.connectionError of
          Nothing -> False
          _       -> True
  in
  div []
      [ input [ id "server"
              , classList [ ("hidden", model.connected)
                          , ("input-error", hadConnectionError) ]
              , autofocus (not model.connected)
              , value model.serverInput
              , on "input" targetValue (UpdateServer >> Signal.message address)
              , onEnter address (Connect model.serverInput)  ]
        [ ]
      , div [ classList [ ("hidden", not model.connected)
                        , ("inline-block", True)
                        ]
            ]
        [ text <| "Connect to: " ++ model.server ]
      , button [ id "toggle-connect"
               , onClick address (if model.connected
                                  then Disconnect Nothing
                                  else Connect model.serverInput )  ]
        [ text <| if model.connected then "Disconnect" else "Connect" ]
      , div [classList [ ("hidden", not hadConnectionError)
                       , ("error", True)
                       , ("inline-block", True) ]]
        [ text <| Maybe.withDefault "" model.connectionError ]
      ]

terminalView : Signal.Address Action -> Model -> Html
terminalView address model =
  let refocusAction = if model.connected then (Refocus "prompt") else NoOp
  in
  div [ id "terminal"
      , classList [ ("hidden", not model.connected && List.isEmpty model.log) ]
      , onClick address refocusAction
      , onBlur address refocusAction
      ]
        ((List.map (logView address) model.log) ++ [promptView address model])

logView address message =
  let isReturn = message.text == "\n"
      tag = if isReturn then div else pre
  in
    tag [ classList [ ("log-entry", True)
                    , ("log-entry-" ++ message.source, True)
                    , ("clearfix", isReturn)
                    , ("float-left", not isReturn) ]]
          [ text  message.text ]

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
signals = Signal.mergeMany
          [ (Signal.map Receive receiveMessage)
          , (Signal.map Disconnect disconnected)
          , actions.signal
          ]

port reFocus : Signal String
port reFocus =
  let prompt = Just "prompt"
      server = Just "server"
      focusId act =
        case act of
          Connect _      ->  prompt
          Send _         ->  prompt
          Receive _      ->  prompt
          UpdateServer _ ->  Nothing
          UpdatePrompt _ ->  Nothing
          Refocus elId    -> Just elId
          _              ->  server
  in
    Signal.filterMap focusId "" signals

port connection : Signal Model
port connection =
  let onConnect (act, model) =
        case act of
          Connect _   -> Just model
          _           -> Nothing
  in
  Signal.map2 (,) signals model
        |> Signal.filterMap onConnect init

port receiveMessage : Signal String
port disconnected : Signal (Maybe String)

port sendMessage : Signal String
port sendMessage =
  let getMessage act =
        case act of
          Send message -> Just message
          _            -> Nothing
  in
    Signal.filterMap getMessage "" actions.signal
