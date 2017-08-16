module Handler.TaskSpec (spec) where

import TestImport
import Data.Aeson (object, encode, decode, (.=))
import Database.Persist.Sql (toSqlKey)
import Data.Maybe
import Network.Wai.Test (SResponse (..))
import Data.Aeson.Types (FromJSON)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.ByteString.Lazy.Internal (ByteString)

sendTaskRequest :: Text -> Int -> Day -> Bool -> Bool -> YesodExample App ()
sendTaskRequest name time deadline happy done = do
    let body = object [ "name"      .= (name :: Text)
                      , "time"      .= (time :: Int)
                      , "deadline"   .= (deadline :: Day)
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

assertJsonResponseIs :: (Show a, Eq a, FromJSON a) => a -> YesodExample App ()
assertJsonResponseIs expected = do
    statusIs 200
    assertHeader "Content-Type" "application/json; charset=utf-8"

    response <- getResponse
    let actual = fromJust $ decode $ simpleBody $ fromJust response
    assertEq "Response should be " expected actual

anEncodedTask :: Data.ByteString.Lazy.Internal.ByteString
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

    describe "getTasksR" $ do

        it "gives a 200" $
            getWithAuthenticatedUser TasksR "foo"
        
        it "gives only the tasks of the autheticated user" $ do
            userBar <- createUser "bar"
            authenticateAs userBar
            sendTaskRequest "bar task" 5 (fromGregorian 2017 06 23) True False

            userBaz <- createUser "bVaz"
            authenticateAs userBaz
            sendTaskRequest "baz task" 5 (fromGregorian 2017 06 4) True False

            -- send a get request
            authenticateAs userBar
            get TasksR

            today <- liftIO $ fmap utctDay getCurrentTime
            let expected = 
                    object [ "tasks" .= [ 
                        object [ "id" .= (1 :: Integer)
                               , "startDate" .= Just today
                               , "name" .= ("bar task" :: Text)
                               , "time"      .= (5 :: Int)
                               , "deadline"  .= (fromGregorian 2017 6 23 :: Day)
                               , "happy"     .= True
                               , "done"      .= False
                               ]
                        ]
                   ]
            assertJsonResponseIs expected

        it "gives only the tasks not done" $ do
            userBar <- createUser "bar"
            authenticateAs userBar
            sendTaskRequest "bar task" 5 (fromGregorian 2017 06 23) True True
            sendTaskRequest "baz task" 5 (fromGregorian 2017 06 4) True False

            get TasksR

            today <- liftIO $ fmap utctDay getCurrentTime
            let expected = 
                  object [ "tasks" .= [ 
                    object [ "id" .= (2 :: Int)
                           , "startDate"  .= Just today
                           , "name" .= ("baz task" :: Text)
                           , "time"      .= (5 :: Int)
                           , "deadline"  .= (fromGregorian 2017 6 4 :: Day)
                           , "happy"     .= True
                           , "done"      .= False
                           ]
                        ]
                  ]
            assertJsonResponseIs expected

    describe "postTasksR" $
        
        it "inserts a task to the user foo" $ do
            let name = "foo task" :: Text
                time = 3 :: Int
                deadline = fromGregorian 2017 06 4 :: Day
                happy = True
                done = False
        
            userBar <- createUser "foo"
            authenticateAs userBar
            sendTaskRequest name time deadline happy done

            statusIs 201
            [Entity _ task] <- runDB $ selectList [TaskName ==. name] []
            assertEq "Should have " task  Task { taskName = name
                                               , taskTime = time
                                               , taskDeadline = deadline
                                               , taskStartDate = Nothing
                                               , taskHappy = happy
                                               , taskDone = done
                                               }
            
    describe "getTaskR" $ do
        
        it "gets a task by Id" $ do
            user <- createUser "foobar"
            authenticateAs user
            sendTaskRequest "foobar task" 7 (fromGregorian 2017 06 5) False True

            -- assuming the database is empty and starting Ids from 1
            get ("/tasks/1" :: Text) 

            let expected = 
                    object [ "task" .=
                        object [ "name" .= ("foobar task" :: Text)
                               , "time"      .= (7 :: Int)
                               , "deadline"  .= (fromGregorian 2017 6 5 :: Day)
                               , "startDate" .= (Nothing :: Maybe Day)
                               , "happy"     .= False
                               , "done"      .= True
                               , "id"        .= (1 :: Int)
                               ]
                   ]
            assertJsonResponseIs expected

        it "returns 404 if the task doesn't exists" $ do
            user <- createUser "foobaz"
            authenticateAs user
            get ("/tasks/2" :: Text)
            statusIs 404

        it "returns 404 if the user doesn't own the task" $ do
            createUser "foo" >>= authenticateAs
            sendTaskRequest "foobar task" 7 (fromGregorian 2017 06 5) False True

            createUser "bar" >>= authenticateAs

            -- assuming the database is empty and starting Ids from 1
            get ("/tasks/1" :: Text) 
            statusIs 404

    describe "putTaskR" $ do
        
        it "inserts a task with id=7 to the user bar" $ do
            user <- createUser "bar"
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
                                               , taskDeadline = fromGregorian 2017 6 8
                                               , taskStartDate = Nothing
                                               , taskHappy = True
                                               , taskDone = False
                                               }
            [Entity _ userTask] <- runDB $ selectList [UserTaskTaskId ==. toSqlKey 7] []
            taskOwner <- runDB $ getJust (userTaskUserId userTask)
            assertEq "User should be " "bar" $ userIdent taskOwner

        it "returns 403 if the user doesn't own the task" $ do
            createUser "owner" >>= authenticateAs
            request $ do
                setMethod "PUT"
                setUrl ("/tasks/11" :: Text)
                setRequestBody anEncodedTask
                addRequestHeader ("Content-Type", "application/json")
            statusIs 201    -- task created with PUT

            createUser "other" >>= authenticateAs
            request $ do
                setMethod "PUT"
                setUrl ("/tasks/11" :: Text)
                setRequestBody anEncodedTask
                addRequestHeader ("Content-Type", "application/json")
            statusIs 403

    describe "deleteTaskR" $ do
        
        it "deletes a task by Id" $ do
            user <- createUser "kill-9"
            authenticateAs user
            sendTaskRequest "2 young 2 die" 11 (fromGregorian 2017 06 9) True True
            statusIs 201    -- task created

            -- assuming the database is empty and starting Ids from 1
            request $ do
                setMethod "DELETE"
                setUrl ("/tasks/1" :: Text)
            statusIs 200    -- task deleted
            
            get ("/tasks/1" :: Text) 
            statusIs 404    -- task not found

        it "don't deletes a task if not the task owner" $ do
            createUser "owner" >>= authenticateAs
            sendTaskRequest "2 young 2 die" 13 (fromGregorian 2017 06 9) True True
            statusIs 201    -- task created

            createUser "not owner" >>= authenticateAs

            -- assuming the database is empty and starting Ids from 1
            request $ do
                setMethod "DELETE"
                setUrl ("/tasks/1" :: Text)
            statusIs 403    -- task not deleted
