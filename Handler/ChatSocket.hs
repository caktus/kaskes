{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.ChatSocket where

import Import
import Yesod.Core
import Yesod.WebSockets
import Data.Text (Text)
import Text.Hamlet (hamletFile)
import Data.Aeson

data SendMessage = SendMessage
  { name    :: Text
  , message :: Text
  , msgType    :: Text
  }

instance ToJSON SendMessage where
  toJSON SendMessage {..} = object
    [ "name"    .= name
    , "message" .= message
    , "msgType" .= msgType
    ]

chatSocket :: WebSocketsT Handler ()
chatSocket = do
  sendTextData $ encode $ serverMessage $
    ("Welcome to kaskes. Please enter your name." :: Text)
  name <- receiveData

  writeChan <- channel <$> getYesod

  sendTextData $ encode $ serverMessage $ "Welcome, " <> name <> "!"
  atomically $ writeTChan writeChan $ toMsg $ serverMessage $ name <> " has joined"

  readChan <- atomically $ dupTChan writeChan

  race_
    (forever $ atomically (readTChan readChan) >>= sendTextData)
    (sourceWS $$ mapM_C (\msg ->
      atomically $ writeTChan writeChan $ toMsg $ SendMessage {
        name = name
      , message = msg
      , msgType = "NEW_MESSAGE"
      }))
  where
    toMsg = toStrict . decodeUtf8 . encode
    serverMessage x = SendMessage {
        name = "server"
      , message = x
      , msgType = "SERVER_MESSAGE"
      }


getChatSocketR :: Handler Html
getChatSocketR = do
  webSockets chatSocket
  withUrlRenderer $(hamletFile "templates/chat.hamlet")
