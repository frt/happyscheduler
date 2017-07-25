module HappyScheduler (scheduleTasks, ScheduledTask(..)) where

import Prelude
import Data.Time (Day(ModifiedJulianDay))
import Data.List (sort)
import Data.Ord (Ord)

import Model

data ScheduledTask = ScheduledTask {
        stStartDate :: Day,
        stTask :: Task
    } deriving (Show, Eq)

instance Ord Task where
    compare t1 t2
        | taskHappy t1 && (not . taskHappy) t2 = LT
        | (not . taskHappy) t1 && taskHappy t2 = GT
        | otherwise = EQ

scheduleTasks :: [Task] -> [ScheduledTask]
scheduleTasks = map (ScheduledTask (ModifiedJulianDay 0)) . sort
