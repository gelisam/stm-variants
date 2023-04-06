{-# LANGUAGE OverloadedStrings #-}

-- | A database with an API which closely matches the "Control.Concurrent.STM"
-- API.
module STM.Variants.DB where

import qualified Database.LevelDB as LevelDB


test :: IO ()
test = do
  (value1, value2) <- LevelDB.runResourceT $ do
    db <- LevelDB.open "example.db" (LevelDB.defaultOptions { LevelDB.createIfMissing = True })
    LevelDB.put db LevelDB.defaultWriteOptions "key1" "value1"
    LevelDB.put db LevelDB.defaultWriteOptions "key2" "value2"
    value1 <- LevelDB.get db LevelDB.defaultReadOptions "key1"
    value2 <- LevelDB.get db LevelDB.defaultReadOptions "key2"
    pure (value1, value2)
  print value1
  print value2
