module HappySchedulerSpec (spec) where
import Test.Hspec
import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay)) 

import Model
import HappyScheduler

sadTask :: Task
sadTask =  Task { taskDone = False
                , taskDueDate = ModifiedJulianDay 0
                , taskHappy = False
                , taskName = "task name"
                , taskTime = 2
                }

happyTask :: Task
happyTask =  Task { taskDone = False
                  , taskDueDate = ModifiedJulianDay 0
                  , taskHappy = True
                  , taskName = "task name"
                  , taskTime = 5
                  }
stHappy, stSad :: ScheduledTask
stSad = ScheduledTask (ModifiedJulianDay 0) sadTask
stHappy = ScheduledTask (ModifiedJulianDay 0) happyTask

spec :: Spec
spec = do 
    describe "HappyScheduler.ScheduledTask" $ do
        it "should have a start date" $
            stStartDate stHappy `shouldBe` ModifiedJulianDay 0
        it "should have a task" $
            stTask stHappy `shouldBe` happyTask
            
    describe "HappyScheduler.scheduleTasks" $ do
        context "when the list of tasks is empty" $
            it "should return an empty list" $ 
                scheduleTasks [] `shouldBe` ([] :: [ScheduledTask])

        context "when the list of tasks is not empty" $
            it "should put happy tasks first" $
                scheduleTasks [sadTask, happyTask] `shouldBe` [stHappy, stSad]

