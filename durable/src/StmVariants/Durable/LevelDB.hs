{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An STM-style API for LevelDB. Thus, like LevelDB, we offer persistence and
-- transactional semantics but only single-process concurrency, not
-- multi-process concurrency.
--
-- For performance, LevelDB only persists to the disk periodically and at the
-- end of the session. Thus, in the event of a power failure, the last few
-- transactions might be lost, but we won't see a partially-executed
-- transaction.
module StmVariants.Durable.LevelDB where

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.State (State)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Typeable (TypeRep, Typeable)
import Data.Unique (Unique)
import Database.LevelDB (DB)
import GHC.Exts (Any)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import qualified Database.LevelDB as LevelDB

import StmVariants.Durable.Internal.TypeableMap


-- | A 'TVar' is a mutable variable which can be read and written to.
data TVar a = TVar DB a


-- | The effect of serializing references.
--
-- The 'Unique' identifies the reference, but only during this session. The
-- 'ByteString' argument is the serialized contents, which overrides previous
-- contents.
class MonadSerialize m where
  serializeRef :: Unique -> ByteString -> m ByteString

-- | The effect of deserializing references.
--
-- The 'Unique' identifies the reference, but only during this session. The
-- 'ByteString' is the serialized contents.
class MonadDeserialize m where
  deserializeRef :: ByteString -> m (Unique, ByteString)


-- | In addition to seralizing values, 'Serializable' also provides a way to
-- serialize references such as 'IORef's and 'TVar's.
class Serializable a where
  serialize
    :: MonadSerialize m
    => a -> m ByteString
  deserialize
    :: MonadDeserialize m
    => ByteString -> m a

-- | Guarantees that writes are complete by the time it returns.
withRootTVar
  :: Serializable a
  => FilePath
  -> a  -- ^ initial value if the file is missing
  -> (TVar a -> ResourceT IO r)
  -> IO r
withRootTVar filePath initialValue body = runResourceT $ do
  db <- LevelDB.open filePath (LevelDB.defaultOptions { LevelDB.createIfMissing = False })
  LevelDB.get db LevelDB.defaultReadOptions ":root" >>= \case
    Nothing -> do
      let initialByteString :: ByteString
          initialByteString = Lazy.toStrict $ Aeson.encode initialValue
      LevelDB.put db LevelDB.defaultWriteOptions ":root" initialByteString
      body $ TVar db initialValue
    Just storedByteString -> do
      case Aeson.decodeStrict storedByteString of
        Nothing -> do
          error $ "withRootTVar: could not decode value "
               ++ show storedByteString
               ++ ", was the database created by a different version of the program?"
        Just storedValue -> do
          body $ TVar db storedValue


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
