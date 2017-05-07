module Handler.Task where

import Import

postTaskR :: Handler ()
postTaskR = do
    task <- requireJsonBody :: Handler Task
    _ <- runDB $ insert task
    sendResponseStatus status201 ("CREATED" :: Text)

-- fake handler only to see the automatic json representation
getTaskR :: Handler Value
getTaskR = do
    let tasks = [Task "mytask" 3 (fromGregorian 2017 05 07) True]
    return $ object ["tasks" .= tasks]
