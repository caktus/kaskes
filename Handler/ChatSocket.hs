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
  sendTextData ("Welcome to Kaskes. Please enter your name." :: Text)
  name <- receiveData
  sendTextData $ encode $ SendMessage {
      name = "server"
    , message = name <> " has joined"
    , msgType = "SERVER_MESSAGE"
    }

  writeChan <- channel <$> getYesod
  readChan <- atomically $ dupTChan writeChan

  race_
    (forever $ do
      text <- atomically $ readTChan readChan
      let msg = SendMessage {
        name = name
      , message = text
      , msgType = "NEW_MESSAGE"
      }
      sendTextData $ encode $ msg)
    (sourceWS $$ mapM_C (\msg ->
      atomically $ writeTChan writeChan msg))


getChatSocketR :: Handler Html
getChatSocketR = do
  webSockets chatSocket
  withUrlRenderer $(hamletFile "templates/chat.hamlet")
