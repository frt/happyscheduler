module HappyScheduler 
    ( scheduleTasks
    ) 
where

import Prelude
import Data.List (sort)
import Data.Ord (Ord)
import Database.Persist (Entity(..))

import Model

newtype MyTask = MyTask (Entity Task) deriving Eq

instance Ord MyTask where
    compare (MyTask (Entity _ t1)) (MyTask (Entity _ t2))
        | taskHappy t1 && (not . taskHappy) t2 = LT
        | (not . taskHappy) t1 && taskHappy t2 = GT
        | otherwise = EQ

scheduleTasks :: [Entity Task] -> [Entity Task]
scheduleTasks = map (\(MyTask t) -> t) . sort . map MyTask
