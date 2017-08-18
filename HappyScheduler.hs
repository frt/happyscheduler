module HappyScheduler 
    ( scheduleTasks
    , Schedulable(..)
    , Deadline
    ) 
where

import Prelude hiding (compare)
import Data.List (sortBy)
import Data.Time.Calendar (Day(ModifiedJulianDay), addDays, diffDays)
import Data.Maybe (isJust, isNothing, fromJust)

type Deadline = Maybe Day

class Schedulable a where
    schHappy :: a -> Bool
    schHappy = not . schSad

    schSad :: a -> Bool
    schSad = not . schHappy

    schTime :: a -> Integer
    schDeadline :: a -> Deadline
    schStartDate :: a -> Maybe Day
    schSetStartDate :: a -> Maybe Day -> a

taskEndDate :: (Schedulable a) => a -> Maybe Day
taskEndDate t
    | isJust (schStartDate t) = Just $ addDays (schTime t) (fromJust $ schStartDate t)
    | otherwise = Nothing
          
daysBetween :: (Schedulable a) => a -> a -> Maybe Integer
daysBetween t1 t2
    | isJust (schStartDate t2) &&
      isJust (taskEndDate t1) = 
            Just $ diffDays (fromJust $ schStartDate t2) (fromJust $ taskEndDate t1)
    | otherwise = Nothing

-- Get the first free day wich have size consecutive days available
-- assumes sorted
firstAvailableDay :: (Schedulable a) => Day -> Integer -> [a] -> Day
firstAvailableDay today _ [] = today
firstAvailableDay today size [t]
    | isNothing (schStartDate t) = firstAvailableDay today size [schSetStartDate t (Just (ModifiedJulianDay 0))]
    | diffDays (fromJust $ schStartDate t) today >= size = today
    | diffDays (fromJust $ taskEndDate t) today < 0 = today
    | otherwise = fromJust $ taskEndDate t
firstAvailableDay today size (t1:t2:ts)
    | isNothing (schStartDate t1) = 
        firstAvailableDay today size $ schSort (schSetStartDate t1 (Just (ModifiedJulianDay 0)):t2:ts)
    | isNothing (schStartDate t2) = 
        firstAvailableDay today size $ schSort (t1:schSetStartDate t2 (Just (ModifiedJulianDay 0)):ts)
    | diffDays (fromJust $ schStartDate t1) today >= size = today
    | diffDays (fromJust $ schStartDate t1) today < 0 = firstAvailableDay today size (t2:ts)
    | fromJust (daysBetween t1 t2) >= size = fromJust (taskEndDate t1)
    | otherwise = firstAvailableDay (fromJust $ taskEndDate t2) size ts

-- Get the last free day wich have size consecutive days available before a deadline
-- assumes sorted
lastAvailableDay :: (Schedulable a) => Day -> Deadline -> Integer -> [a] -> Day
lastAvailableDay today Nothing _ [] = today
lastAvailableDay today Nothing size tasks
    | isJust (schStartDate lastTask) = fromJust $ taskEndDate lastTask
    | otherwise = addDays size today
    where lastTask = last tasks

lastAvailableDay today (Just deadline) size tasks = (lastDay deadline . reverse) tasks
    where lastDay :: (Schedulable a) => Day -> [a] -> Day
          lastDay deadline' []
              | addDays (-size) deadline' >= today = addDays (-size) deadline'
              | otherwise = today

          lastDay deadline' [t]
              | isNothing (schStartDate t) = 
                    lastDay deadline' [schSetStartDate t (Just $ ModifiedJulianDay 0)]
              | addDays (-size) deadline' >= fromJust (taskEndDate t) = addDays (-size) deadline'
              | addDays (-size) (fromJust $ schStartDate t) >= today = 
                    addDays (-size) (fromJust $ schStartDate t)
              | otherwise = fromJust (taskEndDate t)

          lastDay deadline' (t1:t2:ts)
              | isNothing (schStartDate t1) = 
                    lastDay deadline' $ schSort (schSetStartDate t1 (Just $ ModifiedJulianDay 0):t2:ts)
              | addDays (-size) deadline' >= fromJust (taskEndDate t1) = addDays (-size) deadline'
              | isNothing (schStartDate t2) = 
                    lastDay deadline' $ schSort (t1:schSetStartDate t2 (Just $ ModifiedJulianDay 0):ts)
              | addDays (-size) (fromJust $ schStartDate t1) >= fromJust (taskEndDate t2) =
                    addDays (-size) (fromJust $ schStartDate t1)
              | addDays (-size) (fromJust $ schStartDate t2) < today =
                    fromJust (taskEndDate lastTask)
              | otherwise = lastDay (fromJust $ schStartDate t2) (t2:ts)

          lastTask = last tasks
          

schSort :: (Schedulable a) => [a] -> [a]
schSort = sortBy compare
    where
        compare t1 t2
            | schStartDate t1 < schStartDate t2 = LT
            | schStartDate t1 > schStartDate t2 = GT
            | schDeadline t1 `dlLT` schDeadline t2 = LT
            | schDeadline t1 `dlGT` schDeadline t2 = GT
            | schHappy t1 && schSad t2 = LT
            | schSad t1 && schHappy t2 = GT
            | otherwise = EQ
        dlLT (Just _) Nothing = True
        dlLT t1' t2' = t1' < t2'
        dlGT Nothing (Just _) = True
        dlGT t1' t2' = t1' > t2'
    
scheduleTasks :: (Schedulable a) => Day -> [a] -> [a]
scheduleTasks today = schSort . scheduleIterate [] . schSort
    where
          scheduleIterate before [] =  before
          scheduleIterate before (t:ts) = scheduleIterate (before ++ [t']) ts
              where t' = scheduleTask today t (before ++ ts)

scheduleTask :: (Schedulable a) => Day -> a -> [a] -> a
scheduleTask today t ts
    | schHappy t = schSetStartDate t $ Just $ firstAvailableDay today (schTime t) (schSort ts)
    | otherwise = schSetStartDate t $ Just $ lastAvailableDay today (schDeadline t) (schTime t) (schSort ts)
