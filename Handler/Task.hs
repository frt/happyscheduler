module Handler.Task where

import Import

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    task <- runDB $ get404 taskId
    return $ object ["post" .= Entity taskId task]

putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
    task <- requireJsonBody :: Handler Task
    runDB $ replace taskId task
    sendResponseStatus status200 ("UPDATED" :: Text)

deleteTaskR :: TaskId -> Handler Value
deleteTaskR taskId = error "Not yet implemented: deleteTaskR"
