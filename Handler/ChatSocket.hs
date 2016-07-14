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
  sendTextData ("Hello, you!" :: Text)

getChatSocketR :: Handler Html
getChatSocketR = do
  webSockets chatSocket
  defaultLayout [whamlet|No dice, chief!|]
