module Handler.Tasks where

import Import

postTasksR :: Handler ()
postTasksR = do
    task <- requireJsonBody :: Handler Task
    _ <- runDB $ insert task
    sendResponseStatus status201 ("CREATED" :: Text)

-- fake handler only to see the automatic json representation
getTasksR :: Handler Value
getTasksR = do
    let tasks = [Task "mytask" 3 (fromGregorian 2017 05 07) True False]
    return $ object ["tasks" .= tasks]
