module Handler.Task where

import Import

getTasksR :: Handler Value
getTasksR = do
    uid <- requireAuthId
    userTasks <- runDB $ selectList [UserTaskUserId ==. uid] [] :: Handler [Entity UserTask]
    tasks <- runDB $ selectList [TaskId <-. map (\(Entity _ userTask) -> userTaskTaskId userTask) userTasks] [] :: Handler [Entity Task]
    return $ object ["tasks" .= tasks]

postTasksR :: Handler ()
postTasksR = do
    uid <- requireAuthId
    task <- requireJsonBody :: Handler Task
    tid <- runDB $ insert task
    _   <- runDB $ insert (UserTask tid uid)
    sendResponseStatus status201 ("CREATED" :: Text)

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    task <- runDB $ get taskId
    case task of
         Just task' -> return $ object ["task" .= Entity taskId task']
         Nothing -> sendResponseStatus status404 ("Not Found" :: Text)

putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    uid <- requireAuthId
    task <- requireJsonBody :: Handler Task
    maybeTask <- runDB $ get taskId
    case maybeTask of
         Nothing -> do 
             runDB $ insertKey taskId task
             _ <- runDB $ insert (UserTask taskId uid)
             sendResponseStatus status201 ("CREATED" :: Text)
         Just _ -> do
             runDB $ replace taskId task
             sendResponseStatus status200 ("UPDATED" :: Text)

deleteTaskR :: TaskId -> Handler Value
deleteTaskR taskId = do
    uid <-requireAuthId
    [userTaskId] <- runDB $ selectKeysList [UserTaskTaskId ==. taskId, UserTaskUserId ==. uid] []
    runDB $ delete userTaskId
    runDB $ delete taskId
    sendResponseStatus status200 ("DELETED" :: Text)
