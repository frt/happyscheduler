{-# LANGUAGE DeriveGeneric #-}
module HappyScheduler 
    ( scheduleTasks
    , ScheduledTask(..)
    ) 
where

import Prelude
import Data.List (sort)
import Data.Ord (Ord)
import Database.Persist (Entity(..))
import Data.Time.Calendar (Day, addDays)
import Data.Aeson (ToJSON)
import GHC.Generics

import Model

newtype MyTask = MyTask (Entity Task) deriving Eq

instance Ord MyTask where
    compare (MyTask (Entity _ t1)) (MyTask (Entity _ t2))
        | taskHappy t1 && (not . taskHappy) t2 = LT
        | (not . taskHappy) t1 && taskHappy t2 = GT
        | otherwise = EQ

data ScheduledTask = ScheduledTask { scheduledStartDate :: Day
                                   , schTask :: Entity Task
                                   }
    deriving (Show, Eq, Generic)

instance ToJSON ScheduledTask

scheduleTask :: Entity Task -> ScheduledTask
scheduleTask task@(Entity _ t) = ScheduledTask 
                                    (addDays (- (fromIntegral $ taskTime t)) (taskDeadline t)) 
                                    task

scheduleTasks :: [Entity Task] -> [ScheduledTask]
scheduleTasks = map (\(MyTask t) -> scheduleTask t) . sort . map MyTask
