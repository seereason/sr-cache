{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Cache
  ( HasDynamicCache(dynamicCache)
  , CacheMaps
  , dynamicLens
  -- * Generic lens classes
  , anyLens
  , mayLens
  , mapLens
  , atLens
  , atLensM
  -- * Non-generic (overridable) lens classes
  , HasLens(hasLens)
  , HasCache(cacheLens, valueLens, valueLensM)
  -- * Tests
  , tests
  ) where

import Control.Lens (at, Iso', iso, Lens', ReifiedLens', ReifiedLens(Lens))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Stack (HasCallStack)

import Control.Lens (set, view)
import Data.Map.Strict (fromList)
import Test.HUnit

-- | A map from type fingerprint values to wrapped types of that
-- value.  The internals of this ought be hidden to preserve the
-- constraint that this is an Iso between a type and a single value
-- of that type.
type CacheMaps = Map Fingerprint Dynamic

-- | How to find the 'CacheMaps' value.
class HasDynamicCache a where dynamicCache :: Lens' a CacheMaps
instance HasDynamicCache CacheMaps where dynamicCache = id

-- | Given a default, build a lens that points into 'CacheMaps' to a
-- value of any Typeable a.  The value is initially d.
dynamicLens ::
  forall a. (Typeable a, HasCallStack)
  => a -> Lens' CacheMaps a
dynamicLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' CacheMaps (Maybe Dynamic)
    l1 = at (typeRepFingerprint (typeRep (Proxy @a)))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep (Proxy @a)))) . fromDynamic) toDyn

-- | Generic lens, allows access to a single @a@ inside a value @s2.
-- This and other classes in this module are used to break import
-- cycles by allowing the use of s without actually having its
-- declaration.
class (HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens :: HasCallStack => a -> Lens' s a

-- | The generic instance of 'AnyLens'.
instance (HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens a = dynamicCache @s . dynamicLens a

-- | Generic 'Maybe' lens
class AtLens s a where
  mayLens :: Lens' s (Maybe a)

-- | Generic instance of 'AtLens'.
instance AnyLens s (Maybe a) => AtLens s a where
  mayLens = anyLens @s @(Maybe a) Nothing

-- | Generic 'Map' lens.
class (AnyLens s (Map k v), Ord k) => HasMap k v s where
  mapLens :: HasCallStack => Lens' s (Map k v)
  atLens :: HasCallStack => k -> Lens' s (Maybe v)
  -- ^ Accees an element of a map
  atLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

-- | Generic instance of 'HasMap'.
instance (AnyLens s (Map k v), Ord k) => HasMap k v s where
  mapLens = anyLens mempty
  atLens k = mapLens . at k
  atLensM k = do
    k' <- k
    pure $ Lens $ atLens k'

tests :: Test
tests =
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: CacheMaps)
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (atLens @Char @Int 'x') m2)) ]

-- | Like 'AnyLens', but with a default signature so it can be
-- overridden.  The downside is that you need to declare an instance
-- for each pair of types.
class HasLens s a where
  hasLens :: a -> Lens' s a
  default hasLens :: (AnyLens s a, Typeable a) => a -> Lens' s a
  hasLens = anyLens

-- | Like HasMap, but with no generic instance and with default method
-- implementations which can be overridden.
class Ord k => HasCache k v s where
  cacheLens :: HasCallStack => Lens' s (Map k v)
  default cacheLens :: (HasMap k v s, HasCallStack) => Lens' s (Map k v)
  cacheLens = mapLens
  valueLens :: HasCallStack => k -> Lens' s (Maybe v)
  valueLens k = cacheLens . at k
  valueLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))
  valueLensM k = do
    k' <- k
    pure $ Lens $ valueLens k'

instance Ord k => HasCache k v (Map k v) where
  cacheLens = id

cacheMaps :: HasLens s CacheMaps => Lens' s CacheMaps
cacheMaps = hasLens mempty
