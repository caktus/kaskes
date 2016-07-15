{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.ChatSocket where

import Import
import Yesod.Core
import Yesod.WebSockets
import Data.Text (Text)
import Data.Aeson

data SendMessage = SendMessage
  { name    :: Text
  , message :: Text
  }

instance ToJSON SendMessage where
  toJSON SendMessage {..} = object
    [ "name"    .= name
    , "message" .= message
    ]

chatSocket :: WebSocketsT Handler ()
chatSocket = do
  writeChan <- channel <$> getYesod
  readChan <- atomically $ dupTChan writeChan
  race_
    (forever $ do
      text <- atomically $ readTChan readChan
      let msg = SendMessage {
        name = "arb"
      , message = text
      }
      sendTextData $ encode $ msg)
    (sourceWS $$ mapM_C (\msg ->
      atomically $ writeTChan writeChan msg))


getChatSocketR :: Handler Html
getChatSocketR = do
  webSockets chatSocket
  defaultLayout ""
