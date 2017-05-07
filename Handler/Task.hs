module Handler.Task where

import Import

postTaskR :: Handler ()
postTaskR = do
    task <- requireJsonBody :: Handler Task
    _ <- runDB $ insert task
    sendResponseStatus status201 ("CREATED" :: Text)
