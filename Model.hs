{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import HappyScheduler (Schedulable(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance Schedulable (Entity Task) where
    schHappy (Entity _ v) = taskHappy v
    schTime (Entity _ v) = (fromIntegral . taskTime) v
    schDeadline (Entity _ v) = taskDeadline v
    schStartDate (Entity _ v) = taskStartDate v
    schSetStartDate (Entity k v) d = Entity k (v { taskStartDate = d })
