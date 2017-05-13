module Handler.Tasks where

import Import

postTasksR :: Handler ()
postTasksR = do
    task <- requireJsonBody :: Handler Task
    _ <- runDB $ insert task
    sendResponseStatus status201 ("CREATED" :: Text)

getTasksR :: Handler Value
getTasksR = do
    uid <- requireAuthId
    tasks <- runDB $ selectList [TaskUserId ==. uid] [] :: Handler [Entity Task]
    return $ object ["tasks" .= tasks]
