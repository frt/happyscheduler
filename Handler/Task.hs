module Handler.Task where

import Import
import qualified Data.Text as T (pack) 

getTasksR :: Handler Value
getTasksR = do
    uid <- requireAuthId
    tasks <- runDB $ selectList [TaskUserId ==. uid] [] :: Handler [Entity Task]
    return $ object ["tasks" .= tasks]

postTasksR :: Handler ()
postTasksR = do
    uid <- requireAuthId
    task <- requireJsonBody :: Handler Task
    tid <- runDB $ insert task {taskUserId = uid}
    sendResponseStatus status201 (T.pack ("CREATED:task:" ++ show tid))

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    task <- runDB $ get404 taskId
    return $ object ["task" .= Entity taskId task]

putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    task <- requireJsonBody :: Handler Task
    runDB $ replace taskId task
    sendResponseStatus status200 ("UPDATED" :: Text)

deleteTaskR :: TaskId -> Handler Value
deleteTaskR taskId = do
    runDB $ delete taskId
    sendResponseStatus status200 ("DELETED" :: Text)
