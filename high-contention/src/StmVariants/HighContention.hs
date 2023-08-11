-- | A drop-in replacement for the stm API which performs better under high
-- contention, for the common case in which you've already written a lot of code
-- with stm when you discover that contention is an issue.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module StmVariants.HighContention where

import Control.Concurrent.MVar
import Control.Exception (Exception, evaluate, throwIO, try)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Map (Map)
--import qualified Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)


-- Our approach is to first run each transaction in optimistic mode, and only
-- transition to pessimistic mode if contention is detected.


-- | In pessimistic mode, to avoid deadlocks, it is important to acquire locks
-- with a lower 'LockOrder' before acquiring locks with a higher number.
newtype LockOrder = LockOrder
  { unLockOrder :: Int
  } deriving (Enum, Eq, Ord, Show)

minLockOrder
  :: LockOrder
minLockOrder
  = LockOrder 0

{-# NOINLINE globalNextLockOrder #-}
globalNextLockOrder
  :: MVar LockOrder
globalNextLockOrder = unsafePerformIO $ do
  newMVar minLockOrder

newLockOrderIO
  :: IO LockOrder
newLockOrderIO = do
  modifyMVar globalNextLockOrder $ \this -> do
    next <- evaluate $ succ this
    pure (next, this)


-- | If the code attempts to acquire locks in the wrong order, throw an
-- exception so that the transaction can be retried with the correct order.
data InvalidLockOrder = InvalidLockOrder
  { invalidLockOrder_alreadyLocked
      :: LockOrder
  , invalidLockOrder_tryingToLock
      :: LockOrder
  }

-- The convention for exceptions is for 'show' to return the human-readable
-- error-message, not the data constructor.
instance Show InvalidLockOrder where
  show (InvalidLockOrder {..})
    = "InvalidLockOrder: a thread tried to acquire "
   ++ show invalidLockOrder_tryingToLock
   ++ ", but already held "
   ++ show invalidLockOrder_alreadyLocked

instance Exception InvalidLockOrder


-- | A monad which remembers:
-- 1. The highest 'LockOrder' we've acquired so far, so that an 'InvalidLockOrder'
--    exception can be thrown if we try to acquire locks out of order.
-- 2. All the locks the current transaction is interested in (both acquired and
--    not yet acquired), so that we can rollback and acquire all those locks in
--    the right order.
newtype OrderedLocker a = OrderedLocker
  { unOrderedLocker :: ReaderT (IORef OrderedLockerState) IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)
data OrderedLockerState = OrderedLockerState
  { orderedLockerState_highest
      :: Maybe LockOrder
  , orderedLockerState_locks
      :: Set LockOrder
  }

-- | Succeeds if the computation acquires all the locks in order. Otherwise,
-- returns the list of locks the computation was interested in, so that they may
-- be acquired in the right order before trying again.
runOrderedLocker
  :: OrderedLocker a
  -> IO (Either (Set LockOrder) a)
runOrderedLocker body = do
  ref <- newIORef OrderedLockerState
    { orderedLockerState_highest = Nothing
    , orderedLockerState_locks = Set.empty
    }
  resultOrInvalidLockOrder
    <- try
     $ flip runReaderT ref
     $ unOrderedLocker body
  case resultOrInvalidLockOrder of
    Right result -> do
      pure $ Right result
    Left (InvalidLockOrder {}) -> do
      OrderedLockerState _ interestingLocks <- readIORef ref
      pure $ Left interestingLocks

-- | Validate that the transaction is allowed to acquire the lock with the given
-- 'LockOrder', throwing an 'InvalidLockOrder' exception otherwise. Use this
-- before calling a function like 'takeMVar' which acquires a lock.
prepareToAcquireLock
  :: LockOrder
  -> OrderedLocker ()
prepareToAcquireLock lockOrder = do
  prepareToAcquireLocks $ Set.singleton lockOrder

-- | A variant of 'prepareToAcquireLock' which checks multiple locks at once.
-- Prefer using this over calling 'prepareToAcquireLock' multiple times, so that
-- if the transaction aborts in the middle of that loop, 'runOrderedLocker' can
-- report the full set of locks the transaction was interested in.
prepareToAcquireLocks
  :: Set LockOrder
  -> OrderedLocker ()
prepareToAcquireLocks newLocks = OrderedLocker $ do
  ref <- ask
  OrderedLockerState maybeHighest oldLocks <- liftIO $ readIORef ref

  -- Remember the locks we're interested in, so that we can acquire them in the
  -- right order if the transaction aborts.
  let allLocks = Set.union oldLocks newLocks
  liftIO $ writeIORef ref $! OrderedLockerState maybeHighest allLocks

  let next = Set.findMin newLocks
  let newHighest = Set.findMax newLocks
  case maybeHighest of
    Just highest | next < highest -> do
      liftIO $ throwIO $ InvalidLockOrder
        { invalidLockOrder_alreadyLocked = highest
        , invalidLockOrder_tryingToLock = next
        }
    _ -> do
      liftIO $ writeIORef ref $! OrderedLockerState (Just newHighest) allLocks

-- | A variant of 'prepareToAcquireLock' which cannot fail. Use this before
-- using a function like 'tryReadMVar' which reads but does not acquire a lock.
showInterestInLock
  :: LockOrder
  -> OrderedLocker ()
showInterestInLock lockOrder = OrderedLocker $ do
  ref <- ask
  OrderedLockerState highest oldLocks <- liftIO $ readIORef ref
  let allLocks = Set.insert lockOrder oldLocks
  liftIO $ writeIORef ref $! OrderedLockerState highest allLocks


-- | Increments each time the value of a 'TVar' changes.
newtype Version = Version
  { unVersion :: Int
  } deriving (Enum, Eq, Show)

initialVersion
  :: Version
initialVersion
  = Version 0

data TVar a = TVar
  { -- | Holds the current value of the TVar.
    --
    -- In optimistic mode, the value is read and immediately released.
    --
    -- In pessimistic mode, the value is locked until the transaction succeeds
    -- or fails.
    tvar_underlyingMVar
      :: MVar (Version, a)
  , -- | In pessimistic mode, to avoid deadlocks, it is important to acquire locks
    -- with a lower 'tvar_lockOrder' before acquiring locks with a higher number.
    tvar_lockOrder
      :: LockOrder
  }

newtype STM a = STM
  { unSTM :: ReaderT (IORef STMState) OrderedLocker a
  }
  deriving (Functor, Applicative, Monad, MonadIO)
data SomeTVar where
  SomeTVar
    :: forall a
     . { someTVar_underlyingTVar
           :: TVar a
       , someTVar_firstReadVersion
           :: Maybe Version
       , someTVar_lastWrittenValue
           :: Maybe a
       }
    -> SomeTVar
data STMState = STMState
  { stmState_accessedTVars
      :: Map LockOrder SomeTVar
  }

runSTM
  :: STM a
  -> IO a
runSTM
  = undefined

atomically
  :: STM a
  -> IO a
atomically
  = undefined

newTVar
  :: a
  -> STM (TVar a)
newTVar
  = undefined

readTVar
  :: TVar a
  -> STM a
readTVar
  = undefined

writeTVar
  :: TVar a
  -> a
  -> STM ()
writeTVar
  = undefined

retry
  :: STM a
retry
  = undefined
