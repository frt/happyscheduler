module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay)) 
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
                scheduleTasks [] `shouldBe` ([] :: [Entity Task])

        context "when the list of tasks is not empty" $
            it "should put happy tasks first" $
                scheduleTasks [sadTask, happyTask] `shouldBe` [happyTask, sadTask]

