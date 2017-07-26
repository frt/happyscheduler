module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay), addDays) 
import Database.Persist (Entity(..))
import Database.Persist.Sql (toSqlKey)

import Model
import HappyScheduler

sadTask :: Entity Task
sadTask = Entity (toSqlKey 1) Task { 
                 taskDone = False
               , taskDeadline = ModifiedJulianDay 0
               , taskHappy = False
               , taskName = "task name"
               , taskTime = 2
               }

happyTask :: Entity Task
happyTask = Entity (toSqlKey 2) Task { 
                   taskDone = False
                 , taskDeadline = ModifiedJulianDay 0
                 , taskHappy = True
                 , taskName = "task name"
                 , taskTime = 5
                 }

spec :: Spec
spec =
    describe "HappyScheduler.scheduleTasks" $ do
        context "when the list of tasks is empty" $
            it "should return an empty list" $ 
                scheduleTasks [] `shouldBe` ([] :: [ScheduledTask])

        context "when the list of tasks is not empty" $ do
            it "should put happy tasks first" $ do
                let [ScheduledTask _ t1, ScheduledTask _ t2] = scheduleTasks [sadTask, happyTask]
                [t1, t2] `shouldBe` [happyTask, sadTask]

            it "should give a 'start date'" $ do
                let [schTask@(ScheduledTask _ (Entity _ t))] = scheduleTasks [sadTask]
                scheduledStartDate schTask `shouldBe` addDays (- fromIntegral (taskTime t)) (taskDeadline t)
