import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Messages exposing (..)
import Json.Decode exposing (..)


endpoint = "ws://localhost:3000/chat"


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type alias Model =
  { input : String
  , messages: List Message
  }

init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)


-- Update

type Msg =
  Input String
  | Send
  | NewMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages} =
  case msg of
    Input newInput ->
      (Model newInput messages, Cmd.none)
    Send ->
      (Model "" messages, WebSocket.send endpoint input)
    NewMessage str ->
      case decodeString message str of
        Ok newMsg ->
          (Model input (messages ++ [newMsg]), Cmd.none)
        _ ->
          (Model input messages, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen endpoint NewMessage


-- View

view : Model -> Html Msg
view model =
  div [ class "chat-box" ]
    [ div [ class "messages" ] (List.map viewMessage model.messages)
    , Html.form [ class "inputs", onSubmit Send ]
                [ input [onInput Input] [] ]
    ]

viewMessage : Message -> Html msg
viewMessage msg =
  case msg.msgType of
    "SERVER_MESSAGE" ->
      div [ class "message server-message" ] [ text msg.message ]
    "NEW_MESSAGE" ->
      div [ class "message" ]
        [ span [ class "username" ] [ text msg.name ]
        , span [ class "text" ] [ text msg.message ]
        ]
    _ ->
      div [ class "message error" ] [ text ("Bad data received of type: " ++ msg.msgType) ]
