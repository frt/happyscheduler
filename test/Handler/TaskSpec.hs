module Handler.TaskSpec (spec) where

import TestImport
import Data.Aeson (object, encode, (.=))
import Database.Persist.Sql (toSqlKey)
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import HappyScheduler (Deadline)

sendTaskRequest :: Text -> Int -> Deadline -> Bool -> Bool -> YesodExample App ()
sendTaskRequest name time deadline happy done = do
    let body = object [ "name"      .= (name :: Text)
                      , "time"      .= (time :: Int)
                      , "deadline"  .= (deadline :: Deadline)
                      , "happy"     .= (happy :: Bool)
                      , "done"      .= (done :: Bool)
                      ]
        encoded = encode body
    request $ do
        setMethod "POST"
        setUrl TasksR
        setRequestBody encoded
        addRequestHeader ("Content-Type", "application/json")
    return ()

sendPutRequest :: Text -> LBS.ByteString -> YesodExample App ()
sendPutRequest url encodedTask = do
    request $ do
        setMethod "PUT"
        setUrl url
        setRequestBody encodedTask
        addRequestHeader ("Content-Type", "application/json")
    return ()

postTaskAndVerifyDbInsertion :: Text -> Int -> Deadline -> Bool -> Bool -> YesodExample App ()
postTaskAndVerifyDbInsertion name time deadline happy done = do
    user@(Entity uid _) <- createUser "foo"
    authenticateAs user
    sendTaskRequest name time deadline happy done

    statusIs 201
    [Entity _ task] <- runDB $ selectList [TaskName ==. name] []
    assertEq "Should have " task Task { taskName = name
                                      , taskTime = time
                                      , taskDeadline = deadline
                                      , taskStartDate = Nothing
                                      , taskHappy = happy
                                      , taskDone = done
                                      , taskUserId = uid
                                      }

anEncodedTask :: LBS.ByteString
anEncodedTask =
            let name    = "bar task" :: Text
                time    = 5 :: Int
                deadline = fromGregorian 2017 6 8 :: Day
                happy   = True :: Bool
                done    = False :: Bool
                body = object [ "name"      .= name
                              , "time"      .= time
                              , "deadline"   .= deadline
                              , "happy"     .= happy
                              , "done"      .= done
                              ]
                encoded = encode body
            in encoded

spec :: Spec
spec = withApp $ do

    describe "getTasksR" $
        it "gives a 200" $
            getWithAuthenticatedUser TasksR "foo"

    describe "postTasksR" $ do
        
        it "inserts a task to the user foo" $ do
            let name = "foo task" :: Text
                time = 3 :: Int
                deadline = Just (fromGregorian 2017 06 4) :: Deadline
                happy = True
                done = False
            postTaskAndVerifyDbInsertion name time deadline happy done

        it "inserts a task without a deadline" $ do
            let name = "foo task" :: Text
                time = 5 :: Int
                deadline = Nothing :: Deadline
                happy = True
                done = False
            postTaskAndVerifyDbInsertion name time deadline happy done
            
    describe "getTaskR" $ do

        it "returns 404 if the task doesn't exists" $ do
            user <- createUser "foobaz"
            authenticateAs user
            get ("/tasks/2" :: Text)
            statusIs 404

        it "returns 404 if the user doesn't own the task" $ do
            createUser "foo" >>= authenticateAs
            sendTaskRequest "foobar task" 7 (Just $ fromGregorian 2017 06 5) False True

            createUser "bar" >>= authenticateAs

            -- assuming the database is empty and starting Ids from 1
            get ("/tasks/1" :: Text) 
            statusIs 404

    describe "putTaskR" $ do
        
        it "inserts a task with id=7 to the user bar" $ do
            user@(Entity uid _) <- createUser "bar"
            authenticateAs user
            request $ do
                setMethod "PUT"
                setUrl ("/tasks/7" :: Text)
                setRequestBody anEncodedTask
                addRequestHeader ("Content-Type", "application/json")

            statusIs 201
            task <- runDB $ getJust (toSqlKey 7)
            assertEq "Should have the task " task  Task { taskName = "bar task" :: Text
                                               , taskTime = 5 :: Int
                                               , taskDeadline = Just $ fromGregorian 2017 6 8
                                               , taskStartDate = Nothing
                                               , taskHappy = True
                                               , taskDone = False
                                               , taskUserId = uid
                                               }
            taskOwner <- runDB $ getJust (taskUserId task)
            assertEq "User should be " "bar" $ userIdent taskOwner

        it "returns 403 if the user doesn't own the task" $ do
            createUser "owner" >>= authenticateAs
            sendPutRequest ("/tasks/11" :: Text) anEncodedTask
            statusIs 201    -- task created with PUT

            createUser "other" >>= authenticateAs
            sendPutRequest ("/tasks/11" :: Text) anEncodedTask
            statusIs 403

    describe "deleteTaskR" $
        it "don't deletes a task if not the task owner" $ do
            createUser "owner" >>= authenticateAs
            sendTaskRequest "2 young 2 die" 13 (Just $ fromGregorian 2017 06 9) True True
            statusIs 201    -- task created

            createUser "not owner" >>= authenticateAs

            -- assuming the database is empty and starting Ids from 1
            request $ do
                setMethod "DELETE"
                setUrl ("/tasks/1" :: Text)
            statusIs 403    -- task not deleted
