-- | Compatibility module.

{-# LANGUAGE ConstraintKinds #-}
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
{-# OPTIONS -Wall #-}

module Data.Cache
  ( HasDynamicCache
  , anyLens
  , mayLens
  , mapLens
  , atLens
  , atLensM
  , ixLens
  -- * Non-generic (overridable) lens classes
  , New.HasLens(New.hasLens)
  , HasCache(cacheLens, valueLens, valueLensM)
  -- * Tests
  , tests
  ) where

import Control.Lens (at, _Just, Lens', ReifiedLens', ReifiedLens(Lens), Traversal')
import Control.Lens (set, view)
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Data.Map.Strict (fromList)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Test.HUnit
import qualified Data.GenericCache as New

type HasDynamicCache = New.HasGenericCache

-- | Generic lens, allows access to a single @a@ inside a value @s2.
-- This and other classes in this module are used to break import
-- cycles by allowing the use of s without actually having its
-- declaration.
class (New.HasGenericCache s, Typeable a) => AnyLens s a where
  anyLens :: HasCallStack => a -> Lens' s a

-- | The generic instance of 'AnyLens'.
instance (New.HasGenericCache s, Typeable a) => AnyLens s a where
  anyLens = New.anyLens

-- | Generic 'Maybe' lens
class AtLens s a where
  mayLens :: Lens' s (Maybe a)

-- | Generic instance of 'AtLens'.
instance (AnyLens s (Maybe a), Typeable a) => AtLens s a where
  mayLens = New.maybeLens

-- | Generic 'Map' lens.
class (AnyLens s (Map k v), Ord k) => HasMap k v s where
  mapLens :: HasCallStack => Lens' s (Map k v)
  atLens :: HasCallStack => k -> Lens' s (Maybe v)
  -- ^ Accees an element of a map
  atLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

-- | Generic instance of 'HasMap'.
instance (AnyLens s (Map k v), Ord k, Typeable k, Typeable v) => HasMap k v s where
  mapLens = New.mapLens @(Map k v)
  atLens = New.atLens @(Map k v)
  atLensM k = do
    k' <- k
    pure $ Lens $ atLens k'

ixLens :: forall k v s. HasMap k v s => k -> Traversal' s v
ixLens k = atLens k . _Just

tests :: Test
tests =
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: New.GenericCache)
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (atLens @Char @Int 'x') m2)) ]

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
