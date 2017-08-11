module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay), addDays, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Database.Persist.Sql (Entity(..), toSqlKey)

import Model
import HappyScheduler

aTask :: Task
aTask = Task { taskName = "a task"
             , taskStartDate = Nothing
             , taskDone = False
             , taskDeadline = ModifiedJulianDay 0
             , taskHappy = True
             , taskTime = 0
             }

sadTask :: Entity Task
sadTask = Entity (toSqlKey 1) aTask {taskHappy = False}

happyTask :: Entity Task
happyTask = Entity (toSqlKey 1) aTask {taskHappy = True}

today :: IO Day
today = fmap utctDay getCurrentTime

spec :: Spec
spec =
    describe "scheduleTasks" $ do
        context "when the list of tasks is empty" $
            it "should return an empty list" $ do
                today' <- today
                scheduleTasks today' [] `shouldBe` ([] :: [Entity Task])

        it "if everything equal,should put happy tasks first" $ do
            let [Entity _ st1, Entity _ st2] = scheduleTasks (fromGregorian 2017 7 29) [sadTask, happyTask] 
            taskHappy st1 `shouldBe` True
            taskHappy st2 `shouldBe` False

        it "should give a 'start date'" $ do
            today' <- today
            let [Entity _ schTask] = scheduleTasks today' [sadTask { 
                        entityVal = (entityVal sadTask) {taskTime = 1}
                    }]
            taskStartDate schTask `shouldBe` Just today'

        it "should start happy tasks as early as possible" $ do
            today' <- today
            let [Entity _ schTask] = scheduleTasks today' [happyTask]
            taskStartDate schTask `shouldBe` Just today'

        context "when happy tasks schedule superpose" $
            it "should change the less urgent task to start after the more urgent" $ do
                today' <- today
                let t1 = Entity (toSqlKey 1) aTask {
                        taskHappy = True,
                        taskTime = 2,
                        taskDeadline = fromGregorian 2017 8 31
                    }
                    t2 = Entity (toSqlKey 2) aTask {
                        taskHappy = True,
                        taskTime = 3,
                        taskDeadline = fromGregorian 2017 9 1
                    }
                    [Entity _ st1, Entity _ st2] = scheduleTasks today' [t1, t2]

                taskStartDate st1 `shouldBe` Just today'
                taskStartDate st2 `shouldBe` Just (addDays 2 today')

        context "when sad tasks schedule superpose" $
            it "should change the more urgent task to start before the less urgent" $ do
                let t1 = Entity (toSqlKey 3) aTask {
                        taskHappy = False,
                        taskTime = 2,
                        taskDeadline = fromGregorian 2017 8 31
                    }
                    t2 = Entity (toSqlKey 5) aTask {
                        taskHappy = False,
                        taskTime = 3,
                        taskDeadline = fromGregorian 2017 9 1
                    }
                    [Entity _ st1, Entity _ st2] = scheduleTasks (fromGregorian 2017 7 26) [t1, t2]

                taskStartDate st1 `shouldBe` Just (fromGregorian 2017 8 26)
                taskStartDate st2 `shouldBe` Just (fromGregorian 2017 8 29)

        context "when happy and sad tasks schedule superpose" $
            it "should change the sad task to start after the happy task" $ do
                let t1 = Entity (toSqlKey 7) aTask {
                        taskHappy = True,
                        taskTime = 7,
                        taskDeadline = fromGregorian 2017 8 31
                    }
                    t2 = Entity (toSqlKey 11) aTask {
                        taskHappy = False,
                        taskTime = 7,
                        taskDeadline = fromGregorian 2017 8 31
                    }
                    [Entity _ st1, Entity _ st2] = scheduleTasks (fromGregorian 2017 8 20) [t1, t2]

                taskStartDate st1 `shouldBe` Just (fromGregorian 2017 8 20)
                taskStartDate st2 `shouldBe` Just (fromGregorian 2017 8 27)

        it "should sort properly" $ do
            let t1 = Entity (toSqlKey 1) aTask {
                    taskHappy = True,
                    taskTime = 3,
                    taskDeadline = fromGregorian 2017 8 13
                }
                t2 = Entity (toSqlKey 2) aTask {
                    taskHappy = True,
                    taskTime = 5,
                    taskDeadline = fromGregorian 2017 8 13
                }
                t3 = Entity (toSqlKey 3) aTask {
                    taskHappy = False,
                    taskTime = 2,
                    taskDeadline = fromGregorian 2017 8 7
                }
                [st1, st2, st3] = scheduleTasks (fromGregorian 2017 8 1) [t1, t2, t3]
            st1 `shouldBe` t1 { entityVal = 
                                (entityVal t1) {taskStartDate = Just (fromGregorian 2017 8 1)} 
                              }
            st2 `shouldBe` t3 { entityVal = 
                                (entityVal t3) {taskStartDate = Just (fromGregorian 2017 8 5)} 
                              }
            st3 `shouldBe` t2 { entityVal = 
                                (entityVal t2) {taskStartDate = Just (fromGregorian 2017 8 7)} 
                              }
