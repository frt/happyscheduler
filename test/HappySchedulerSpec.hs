module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay), addDays, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

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

today :: IO Day
today = fmap utctDay getCurrentTime

spec :: Spec
spec =
    describe "scheduleTasks" $ do
        context "when the list of tasks is empty" $
            it "should return an empty list" $ do
                today' <- today
                scheduleTasks today' [] `shouldBe` ([] :: [Task])

        it "should put happy tasks first" $ do
            today' <- today
            scheduleTasks today' [sadTask, happyTask] 
                `shouldBe` [happyTask {taskStartDate = today'}, sadTask]

        it "should give a 'start date'" $ do
            today' <- today
            let [schTask] = scheduleTasks today' [sadTask { 
                        taskFromModel = (taskFromModel sadTask) { Model.taskTime = 1 }
                    }]
            taskStartDate schTask `shouldBe` ModifiedJulianDay (-1)

        it "should start happy tasks as early as possible" $ do
            today' <- today
            let [schTask] = scheduleTasks today' [happyTask]
            taskStartDate schTask `shouldBe` today'

        context "when happy tasks schedule superpose" $
            it "should change the less urgent task to start after the more urgent" $ do
                today' <- today
                let t1 = aTask { taskFromModel = modelTask { 
                        Model.taskHappy = True,
                        Model.taskTime = 2,
                        Model.taskDeadline = fromGregorian 2017 8 31 } 
                    }
                    t2 = aTask { taskFromModel = modelTask {
                        Model.taskHappy = True,
                        Model.taskTime = 3,
                        Model.taskDeadline = fromGregorian 2017 9 1 } 
                    }
                    [st1, st2] = scheduleTasks today' [t1, t2]

                taskStartDate st1 `shouldBe` today'
                taskStartDate st2 `shouldBe` addDays 2 today'

        context "when sad tasks schedule superpose" $
            it "should change the more urgent task to start before the less urgent" $ do
                let t1 = aTask { taskFromModel = modelTask { 
                        Model.taskHappy = False,
                        Model.taskTime = 2,
                        Model.taskDeadline = fromGregorian 2017 8 31 } 
                    }
                    t2 = aTask { taskFromModel = modelTask {
                        Model.taskHappy = False,
                        Model.taskTime = 3,
                        Model.taskDeadline = fromGregorian 2017 9 1 } 
                    }
                    [st1, st2] = scheduleTasks (fromGregorian 2017 7 28) [t1, t2]

                taskStartDate st1 `shouldBe` fromGregorian 2017 8 27
                taskStartDate st2 `shouldBe` fromGregorian 2017 8 29
