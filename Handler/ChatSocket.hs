{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.ChatSocket where

import Import
import Yesod.Core
import Yesod.WebSockets
import Data.Text (Text)

chatSocket :: WebSocketsT Handler ()
chatSocket = do
  writeChan <- channel <$> getYesod
  readChan <- atomically $ dupTChan writeChan
  race_
    (forever $ atomically (readTChan readChan) >>= sendTextData)
    (sourceWS $$ mapM_C (\msg ->
      atomically $ writeTChan writeChan msg))


getChatSocketR :: Handler Html
getChatSocketR = do
  webSockets chatSocket
  defaultLayout [whamlet|No dice, chief!|]
