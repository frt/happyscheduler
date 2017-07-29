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

scheduleTask :: Day -> Task -> Task
scheduleTask today task
    | isHappy task = task { taskStartDate = today }
    | otherwise    = task { taskStartDate = addDays (-taskTime task) taskDeadline }
    where 
          taskDeadline = (Model.taskDeadline . taskFromModel) task

resolveSuperposed :: [Task] -> [Task]
resolveSuperposed [] = []
resolveSuperposed [x] = [x]
resolveSuperposed (t1:t2:ts) 
    | isHappy t1 && isHappy t2 && superposed = t1 : resolveSuperposed (t2 {taskStartDate = 
                                                                            taskEndDate t1}:ts)
    | otherwise = t1 : resolveSuperposed (t2:ts)
    where superposed
            | taskStartDate t2 <= taskEndDate t1 = True
            | otherwise = False
          taskEndDate t = addDays (taskTime t) (taskStartDate t)

resolveSuperposedSad :: [Task] -> [Task]
resolveSuperposedSad = reverse . resolveSuperposedSad' . reverse
    where
        resolveSuperposedSad' [] = []
        resolveSuperposedSad' [x] = [x]
        resolveSuperposedSad' (t1:t2:ts)
            | isSad t1 && isSad t2 && superposed = 
                t1 : resolveSuperposedSad' 
                        (t2 {taskStartDate = addDays (-taskTime t2) (taskStartDate t1)}:ts)
            | otherwise = t1 : resolveSuperposedSad' (t2:ts)
            where superposed
                    | taskEndDate t2 >= taskStartDate t1 = True
                    | otherwise = False
                  taskEndDate t = addDays (taskTime t) (taskStartDate t)

scheduleTasks :: Day -> [Task] -> [Task]
scheduleTasks today = resolveSuperposedSad . resolveSuperposed . map (scheduleTask today) . sort
