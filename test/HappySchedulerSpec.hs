module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay), addDays) 

import qualified Model
import HappyScheduler

modelTask = Model.Task { taskName = "a task"
                       , taskDone = False
                       , taskDeadline = ModifiedJulianDay 0
                       , taskHappy = True
                       , taskTime = 0
                       }


aTask :: Task
aTask = Task { taskId = 1
             , taskStartDate = ModifiedJulianDay 0
             , taskFromModel = modelTask
             }

sadTask :: Task
sadTask = aTask {taskFromModel = modelTask {Model.taskHappy = False}}

happyTask :: Task
happyTask = aTask {taskFromModel = modelTask {Model.taskHappy = True}}

spec :: Spec
spec =
    describe "HappyScheduler.scheduleTasks" $ do
        context "when the list of tasks is empty" $
            it "should return an empty list" $ 
                scheduleTasks [] `shouldBe` ([] :: [Task])

        context "when the list of tasks is not empty" $ do
            it "should put happy tasks first" $
                scheduleTasks [sadTask, happyTask] `shouldBe` [happyTask, sadTask]

            it "should give a 'start date'" $ do
                let [schTask] = scheduleTasks [aTask {taskFromModel = modelTask {Model.taskTime = 1}} ]
                taskStartDate schTask `shouldBe` ModifiedJulianDay (-1)
