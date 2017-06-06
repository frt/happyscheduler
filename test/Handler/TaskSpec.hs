module Handler.TaskSpec (spec) where

import TestImport
import Data.Aeson (object, encode, decode, (.=))
--import qualified Database.Persist as DB (get)
import Data.Maybe
import Network.Wai.Test (SResponse (..))
import Data.Aeson.Types (FromJSON)

sendTaskRequest :: Text -> Int -> Day -> Bool -> Bool -> YesodExample App ()
sendTaskRequest name time dueDate happy done = do
    let body = object [ "name"      .= (name :: Text)
                      , "time"      .= (time :: Int)
                      , "dueDate"   .= (dueDate :: Day)
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

spec :: Spec
spec = withApp $ do

    describe "getTasksR" $ do
        it "gives a 200" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity
            get TasksR
            statusIs 200
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

            let expected = 
                    object [ "tasks" .= [ 
                        object [ "name" .= ("bar task" :: Text)
                               , "time"      .= (5 :: Int)
                               , "dueDate"   .= (fromGregorian 2017 6 23 :: Day)
                               , "happy"     .= True
                               , "done"      .= False
                               , "id"        .= (1 :: Int)
                               ]
                        ]
                   ]
            assertJsonResponseIs expected

    describe "postTasksR" $
        it "inserts user a task to the user foo" $ do
            let name = "foo task" :: Text
                time = 3 :: Int
                dueDate = fromGregorian 2017 06 4 :: Day
                happy = True
                done = False
        
            userBar <- createUser "foo"
            authenticateAs userBar
            sendTaskRequest name time dueDate happy done

            statusIs 201
            [Entity _ task] <- runDB $ selectList [TaskName ==. name] []
            assertEq "Should have " task  Task { taskName = name
                                               , taskTime = time
                                               , taskDueDate = dueDate
                                               , taskHappy = happy
                                               , taskDone = done
                                               }
            
    describe "getTaskR" $ 
        it "Should get a task by Id." $ do
            user <- createUser "foobar"
            authenticateAs user
            sendTaskRequest "foobar task" 7 (fromGregorian 2017 06 5) False True

            -- assuming the database is empty and starting Ids from 1
            get ("/tasks/1" :: Text) 

            let expected = 
                    object [ "task" .=
                        object [ "name" .= ("foobar task" :: Text)
                               , "time"      .= (7 :: Int)
                               , "dueDate"   .= (fromGregorian 2017 6 5 :: Day)
                               , "happy"     .= False
                               , "done"      .= True
                               , "id"        .= (1 :: Int)
                               ]
                   ]
            assertJsonResponseIs expected

    describe "putTaskR" $ 
        it "Spec not implemented: putTaskR" $ 
            const pending

    describe "deleteTaskR" $ 
        it "Spec not implemented: deleteTaskR" $
            const pending
