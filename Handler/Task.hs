module Handler.Task where

import Import

import HappyScheduler

getTasksR :: Handler Value
getTasksR = do
    uid <- requireAuthId
    tasks <- runDB $ selectList 
        [ TaskDone ==. False
        , TaskUserId ==. uid
        ] [] :: Handler [Entity Task]

    today <- liftIO $! fmap utctDay getCurrentTime

    return $ object ["tasks" .= scheduleTasks today tasks]

getTasksDoneR :: Handler Value
getTasksDoneR = do
    uid <- requireAuthId
    tasks <- runDB $ selectList 
        [ TaskDone ==. True
        , TaskUserId ==. uid
        ] [] :: Handler [Entity Task]

    return $ object ["tasks" .= tasks]

postTasksR :: Handler ()
postTasksR = do
    uid <- requireAuthId
    task <- requireJsonBody :: Handler Task
    _ <- runDB $ insert task { taskUserId = uid }
    sendResponseStatus status201 ("CREATED" :: Text)

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    -- user verification
    uid <-requireAuthId
    tasks <- runDB $ selectList [TaskId ==. taskId, TaskUserId ==. uid] [] :: Handler [Entity Task]
    case tasks of
        [task] -> return $ object ["task" .= task]
        _ -> sendResponseStatus status404 ("Not Found" :: Text)

putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    uid <- requireAuthId
    task <- requireJsonBody :: Handler Task
    maybeTask <- runDB $ get taskId
    case maybeTask of
         Nothing -> do 
             runDB $ insertKey taskId (task {taskUserId = uid})
             sendResponseStatus status201 ("CREATED" :: Text)
         Just task' ->
             if taskUserId task' == uid
                 then do runDB $ replace taskId (task {taskUserId = uid})
                         sendResponseStatus status200 ("UPDATED" :: Text)

                 else sendResponseStatus status403 ("You can't do it." :: Text)

deleteTaskR :: TaskId -> Handler Value
deleteTaskR taskId = do
    uid <-requireAuthId
    tasks <- runDB $
        selectKeysList [TaskId ==. taskId, TaskUserId ==. uid] []
    case tasks of
        [_] -> do
            runDB $ delete taskId
            sendResponseStatus status200 ("DELETED" :: Text)
        _ -> sendResponseStatus status403 ("You can't do it." :: Text)
