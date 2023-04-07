{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module STM.Variants.LevelDB.TypeableMap where

import Data.Map (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map as Map


-- | A 'Map' whose keys can have any type.
newtype TypeableMap a = TypeableMap
  { unTypeableMap :: Map TypeRep Any  -- ^ Map (k :: Type) (Map k a)
  }

empty
  :: TypeableMap a
empty
  = TypeableMap Map.empty

insert
  :: forall k a. (Ord k, Typeable k)
  => k
  -> a
  -> TypeableMap a
  -> TypeableMap a
insert k a (TypeableMap maps)
  = TypeableMap $ Map.alter go t maps
  where
    t :: TypeRep
    t = typeRep (Proxy @k)

    go :: Maybe Any -> Maybe Any
    go Nothing
      = Just $ unsafeCoerce $ Map.singleton k a
    go (Just anyMap)
      = Just $ unsafeCoerce $ Map.insert k a $ unsafeCoerce anyMap

lookup
  :: forall k a. (Ord k, Typeable k)
  => k
  -> TypeableMap a
  -> Maybe a
lookup k (TypeableMap maps)
  = case Map.lookup t maps of
      Nothing
        -> Nothing
      Just anyMap
        -> Map.lookup k $ unsafeCoerce anyMap
  where
    t :: TypeRep
    t = typeRep (Proxy @k)

delete
  :: forall k a. (Ord k, Typeable k)
  => k
  -> TypeableMap a
  -> TypeableMap a
delete k (TypeableMap maps)
  = TypeableMap $ Map.alter go t maps
  where
    t :: TypeRep
    t = typeRep (Proxy @k)

    go :: Maybe Any -> Maybe Any
    go Nothing
      = Nothing
    go (Just anyMap)
      = let anyMap' = Map.delete k $ unsafeCoerce anyMap
     in if Map.null anyMap'
        then Nothing
        else Just $ unsafeCoerce anyMap'