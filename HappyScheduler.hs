{-# LANGUAGE DeriveGeneric #-}
module HappyScheduler 
    ( scheduleTasks
    , Task(..)
    ) 
where

import Prelude
import Data.List (sort)
import Data.Ord (Ord)
import Data.Time.Calendar (Day, addDays)
import Data.Aeson (ToJSON)
import GHC.Generics

-- lets use the model from the application
import qualified Model

instance Ord Task where
    compare t1 t2
        | isHappy t1 && (not . isHappy) t2 = LT
        | (not . isHappy) t1 && isHappy t2 = GT
        | otherwise = EQ
            where isHappy = Model.taskHappy . taskFromModel

data Task = Task { taskId :: Integer
                 , taskStartDate :: Day
                 , taskFromModel :: Model.Task
                 } deriving (Show, Eq, Generic)

instance ToJSON Task

scheduleTask :: Task -> Task
scheduleTask task = task { taskStartDate = addDays (-taskTime) taskDeadline }
    where taskTime = fromIntegral $ (Model.taskTime . taskFromModel) task
          taskDeadline = (Model.taskDeadline . taskFromModel) task

scheduleTasks :: [Task] -> [Task]
scheduleTasks = map scheduleTask . sort
