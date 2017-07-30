module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay), addDays, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

import qualified Model
import HappyScheduler

modelTask :: Model.Task
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

        it "if everything equal,should put happy tasks first" $ do
            let [st1, st2] = scheduleTasks (fromGregorian 2017 7 29) [sadTask, happyTask] 
            isHappy st1 `shouldBe` True
            isHappy st2 `shouldBe` False

        it "should give a 'start date'" $ do
            today' <- today
            let [schTask] = scheduleTasks today' [sadTask { 
                        taskFromModel = (taskFromModel sadTask) { Model.taskTime = 1 }
                    }]
            taskStartDate schTask `shouldBe` today'

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
                    [st1, st2] = scheduleTasks (fromGregorian 2017 7 26) [t1, t2]

                taskStartDate st1 `shouldBe` fromGregorian 2017 8 26
                taskStartDate st2 `shouldBe` fromGregorian 2017 8 29

        context "when happy and sad tasks schedule superpose" $
            it "should change the sad task to start after the happy task" $ do
                let t1 = aTask { taskFromModel = modelTask { 
                        Model.taskHappy = True,
                        Model.taskTime = 7,
                        Model.taskDeadline = fromGregorian 2017 8 31 } 
                    }
                    t2 = aTask { taskFromModel = modelTask {
                        Model.taskHappy = False,
                        Model.taskTime = 7,
                        Model.taskDeadline = fromGregorian 2017 8 31 } 
                    }
                    [st1, st2] = scheduleTasks (fromGregorian 2017 8 20) [t1, t2]

                taskStartDate st1 `shouldBe` fromGregorian 2017 8 20
                taskStartDate st2 `shouldBe` fromGregorian 2017 8 27

        it "should sort properly" $ do
            let t1 = aTask { taskId = 1, taskFromModel = modelTask { 
                    Model.taskHappy = True,
                    Model.taskTime = 3,
                    Model.taskDeadline = fromGregorian 2017 8 13 } 
                }
                t2 = aTask { taskId = 2, taskFromModel = modelTask {
                    Model.taskHappy = True,
                    Model.taskTime = 5,
                    Model.taskDeadline = fromGregorian 2017 8 13 } 
                }
                t3 = aTask { taskId = 3, taskFromModel = modelTask {
                    Model.taskHappy = False,
                    Model.taskTime = 2,
                    Model.taskDeadline = fromGregorian 2017 8 7 } 
                }
                [st1, st2, st3] = scheduleTasks (fromGregorian 2017 8 1) [t1, t2, t3]
            st1 `shouldBe` t1 {taskStartDate = fromGregorian 2017 8 1}
            st2 `shouldBe` t3 {taskStartDate = fromGregorian 2017 8 5}
            st3 `shouldBe` t2 {taskStartDate = fromGregorian 2017 8 7}
