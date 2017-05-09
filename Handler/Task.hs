module Handler.Task where

import Import

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
    task <- runDB $ get404 taskId
    return $ object ["post" .= Entity taskId task]

putTaskR :: TaskId -> Handler Html
putTaskR taskId = error "Not yet implemented: putTaskR"

deleteTaskR :: TaskId -> Handler Html
deleteTaskR taskId = error "Not yet implemented: deleteTaskR"
