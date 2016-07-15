module Handler.Home where

import Import
import Text.Hamlet (hamletFile)

getHomeR :: Handler Html
getHomeR = do
    withUrlRenderer $(hamletFile "templates/chat.hamlet")
