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
        | isHappy t1 && isSad t2 = LT
        | isSad t1 && isHappy t2 = GT
        | otherwise = compare (deadline t1) (deadline t2)
        where deadline = Model.taskDeadline . taskFromModel

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

scheduleTask :: Day -> Task -> Task
scheduleTask today task
    | isHappy task = task { taskStartDate = today }
    | otherwise    = task { taskStartDate = addDays (-taskTime task) taskDeadline }
    where 
          taskDeadline = (Model.taskDeadline . taskFromModel) task

tasksSuperpose t1 t2
    | startIsIn t1 t2 = True
    | endIsIn t1 t2 = True
    | startIsIn t2 t1 = True
    | endIsIn t2 t1 = True
    | otherwise = False
    where startIsIn t t' = taskStartDate t' <= taskStartDate t
                             && taskStartDate t <= taskEndDate t'
          endIsIn t t' = taskStartDate t' <= taskEndDate t
                           && taskEndDate t <= taskEndDate t'
          

insertTaskInSchedule :: Task -> [Task] -> [Task]
insertTaskInSchedule t [] = [t]
insertTaskInSchedule t (t':ts)
    | t <= t' = t : insertTaskInSchedule t' ts
    | tasksSuperpose t t' = t' : t {taskStartDate = taskEndDate t'} : ts
    | otherwise = t' : t : ts

preScheduleHappy :: [Task] -> [Task]
preScheduleHappy [] = []
preScheduleHappy [x] = [x]
preScheduleHappy (t1:t2:ts)
    | isHappy t1 && isHappy t2 && superposed = t1 : preScheduleHappy (t2 {taskStartDate = 
                                                                            taskEndDate t1}:ts)
    | otherwise = t1 : preScheduleHappy (t2:ts)
    where superposed
            | taskStartDate t2 <= taskEndDate t1 = True
            | otherwise = False

preScheduleSad :: [Task] -> [Task]
preScheduleSad = reverse . preScheduleSad' . reverse
    where
        preScheduleSad' [] = []
        preScheduleSad' [x] = [x]
        preScheduleSad' (t1:t2:ts)
            | isSad t1 && isSad t2 && superposed = 
                t1 : preScheduleSad' 
                        (t2 {taskStartDate = addDays (-taskTime t2) (taskStartDate t1)}:ts)
            | otherwise = t1 : preScheduleSad' (t2:ts)
            where superposed
                    | taskEndDate t2 >= taskStartDate t1 = True
                    | otherwise = False

scheduleTasks :: Day -> [Task] -> [Task]
scheduleTasks today = rearrangeAll . preScheduleSad . preScheduleHappy
                        . map (scheduleTask today) . sort
    where rearrangeAll = foldl (flip insertTaskInSchedule) []
