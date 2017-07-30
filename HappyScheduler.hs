{-# LANGUAGE DeriveGeneric #-}
module HappyScheduler 
    ( scheduleTasks
    , Task(..)
    , isHappy
    ) 
where

import Prelude
import Data.List (sort)
import Data.Ord (Ord)
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Aeson (ToJSON)
import GHC.Generics

-- lets use the model from the application
import qualified Model

instance Ord Task where
    compare t1 t2
        | taskStartDate t1 < taskStartDate t2       = LT
        | taskStartDate t1 > taskStartDate t2       = GT
        | taskDeadline t1 < taskDeadline t2       = LT
        | taskDeadline t1 > taskDeadline t2       = GT
        | isHappy t1 && isSad t2 = LT
        | isSad t1 && isHappy t2 = GT
        | otherwise = EQ

data Task = Task { taskId :: Integer
                 , taskStartDate :: Day
                 , taskFromModel :: Model.Task
                 } deriving (Show, Eq, Generic)

instance ToJSON Task

isHappy :: Task -> Bool
isHappy = Model.taskHappy . taskFromModel

isSad :: Task -> Bool
isSad = not . isHappy

taskTime :: Task -> Integer
taskTime = fromIntegral . Model.taskTime . taskFromModel

taskEndDate :: Task -> Day
taskEndDate t = addDays (taskTime t) (taskStartDate t)

taskDeadline :: Task -> Day
taskDeadline = Model.taskDeadline . taskFromModel
          
daysBetween :: Task -> Task -> Integer
daysBetween t1 t2 = diffDays (taskStartDate t2) (taskEndDate t1)

-- Get the first free day wich have size consecutive days available
-- assumes sorted
firstAvailableDay :: Day -> Integer -> [Task] -> Day
firstAvailableDay today _ [] = today
firstAvailableDay today size [t]
    | diffDays (taskStartDate t) today >= size = today
    | diffDays (taskEndDate t) today < 0 = today
    | otherwise = taskEndDate t
firstAvailableDay today size (t1:t2:ts)
    | diffDays (taskStartDate t1) today >= size = today
    | diffDays (taskStartDate t1) today < 0 = firstAvailableDay today size (t2:ts)
    | daysBetween t1 t2 >= size = taskEndDate t1
    | otherwise = firstAvailableDay (taskEndDate t2) size ts

-- Get the last free day wich have size consecutive days available before a deadline
-- assumes sorted
lastAvailableDay :: Day -> Day -> Integer -> [Task] -> Day
lastAvailableDay today deadline size []
    | addDays (-size) deadline >= today = addDays (-size) deadline
    | otherwise = today
lastAvailableDay today deadline size [t]
    | addDays (-size) deadline >= taskEndDate t = addDays (-size) deadline
    | addDays (-size) (taskStartDate t) >= today = addDays (-size) (taskStartDate t)
    | otherwise = taskEndDate t
lastAvailableDay today deadline size tasks = (lastDay deadline . reverse) tasks
    where lastDay deadline' []
              | addDays (-size) deadline' >= today = addDays (-size) deadline'
              | otherwise = today
          lastDay deadline' [t]
              | addDays (-size) deadline' >= taskEndDate t = addDays (-size) deadline'
              | addDays (-size) (taskStartDate t) >= today = addDays (-size) (taskStartDate t)
              | otherwise = taskEndDate t
          lastDay deadline' (t1:t2:ts)
              | addDays (-size) deadline' >= taskEndDate t1 = addDays (-size) deadline'
              | addDays (-size) (taskStartDate t1) >= taskEndDate t2 = addDays (-size) (taskStartDate t1)
              | addDays (-size) (taskStartDate t2) < today = taskEndDate lastTask
              | otherwise = lastDay (taskStartDate t2) (t2:ts)

          lastTask = last tasks
    
scheduleTasks :: Day -> [Task] -> [Task]
scheduleTasks today = sort . scheduleIterate [] . sort
    where
          scheduleIterate before [] =  before
          scheduleIterate before (t:ts) = scheduleIterate (before ++ [t']) ts
              where t' = scheduleTask today t (before ++ ts)

scheduleTask :: Day -> Task -> [Task] -> Task
scheduleTask today t ts
    | isHappy t = t { taskStartDate = firstAvailableDay today (taskTime t) (sort ts) }
    | otherwise = t { taskStartDate = lastAvailableDay today (taskDeadline t) (taskTime t) (sort ts) }
