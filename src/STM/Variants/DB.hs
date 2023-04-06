{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A database with an API which closely matches the "Control.Concurrent.STM"
-- API.
module STM.Variants.DB where

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString (ByteString)
import Database.LevelDB (DB)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Database.LevelDB as LevelDB


-- | A 'PVar' is a mutable variable which can be read and written to.
data PVar a = PVar DB a


-- | 'Serializable' is a synonym for 'Aeson.FromJSON' and 'Aeson.ToJSON'.
type Serializable a = (Aeson.FromJSON a, Aeson.ToJSON a)

-- | Guarantees that writes are complete by the time it returns.
withRootPVar
  :: Serializable a
  => FilePath
  -> a  -- ^ initial value if the file is missing
  -> (PVar a -> ResourceT IO r)
  -> IO r
withRootPVar filePath initialValue body = runResourceT $ do
  db <- LevelDB.open filePath (LevelDB.defaultOptions { LevelDB.createIfMissing = False })
  LevelDB.get db LevelDB.defaultReadOptions ":root" >>= \case
    Nothing -> do
      let initialByteString :: ByteString
          initialByteString = Lazy.toStrict $ Aeson.encode initialValue
      LevelDB.put db LevelDB.defaultWriteOptions ":root" initialByteString
      body $ PVar db initialValue
    Just storedByteString -> do
      case Aeson.decodeStrict storedByteString of
        Nothing -> do
          error $ "withRootPVar: could not decode value "
               ++ show storedByteString
               ++ ", was the database created by a different version of the program?"
        Just storedValue -> do
          body $ PVar db storedValue


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
