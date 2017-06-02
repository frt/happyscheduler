module Handler.TaskSpec (spec) where

import TestImport
import Data.Aeson (object, encode, (.=))
import qualified Database.Persist as DB (get)

spec :: Spec
spec = withApp $ do

    describe "getTasksR" $
        it "gives a 200" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity
            get TasksR
            statusIs 200

    describe "postTasksR" $
        it "inserts user a task to the user foo" $ do
            let name = "foo task" :: Text
                time = 3 :: Int
                dueDate = fromGregorian 2017 06 4 :: Day
                happy = True
                done = False
        
                body = object [ "name"      .= name
                              , "time"      .= time 
                              , "dueDate"   .= dueDate
                              , "happy"     .= happy
                              , "done"      .= done
                              ]
                encoded = encode body
            
            -- dummy login
            userEntity <- createUser "foo"
            authenticateAs userEntity

            request $ do
                setMethod "POST"
                setUrl TasksR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")
            statusIs 201

            [Entity _ task] <- runDB $ selectList [TaskName ==. name] []
            assertEq "Should have " task  Task { taskName = name
                                               , taskTime = time
                                               , taskDueDate = dueDate
                                               , taskHappy = happy
                                               , taskDone = done
                                               }
            
    describe "getTaskR" $ 
        it "Spec not implemented: getTaskR" $ 
            const pending

    describe "putTaskR" $ 
        it "Spec not implemented: putTaskR" $ 
            const pending

    describe "deleteTaskR" $ 
        it "Spec not implemented: deleteTaskR" $
            const pending
