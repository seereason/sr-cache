-- | Use Dynamic to create a cache that requires no declarations - you
-- can write @atLens key .= Just value@ and it just works.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Data.Cache
  ( Dyn.DynamicCache
  , Dyn.HasDynamicCache(dynamicCache)
  , anyLens
  , maybeLens
  , mapLens
  , atLens
  , atLensM
  , ixLens
  , boundedLens
  , defaultLens
  , monoidLens
  -- * Non-generic (overridable) lens classes
  , HasLens(hasLens)
  -- * Tests
  , tests
  ) where

import Control.Lens (At(at), Index, IxValue, Iso', iso, _Just, Lens', non,
                     ReifiedLens', ReifiedLens(Lens), set, Traversal', view)
import qualified Data.Cache.Dynamic as Dyn
import Data.Default (Default(def))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (fromList, Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Test.HUnit
import Type.Reflection

-- | Generic lens, allows access to a single @a@ inside a value @s2.
-- This and other classes in this module are used to break import
-- cycles by allowing the use of s without actually having its
-- declaration.
class (Dyn.HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens :: HasCallStack => a -> Lens' s a

-- | The generic instance of 'AnyLens'.
instance (Dyn.HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens = Dyn.anyLens

-- | Generic 'Maybe' lens
class MaybeLens s a where
  maybeLens :: Lens' s (Maybe a)

-- | Generic instance of 'AtLens'.
instance (AnyLens s (Maybe a), Typeable a) => MaybeLens s a where
  maybeLens = Dyn.maybeLens

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (Dyn.HasDynamicCache s, Typeable a, Default a) => Lens' s a
defaultLens = Dyn.anyLens @a @s def

-- | A lens to any 'Bounded' instance with default 'minBound'
boundedLens ::
  forall map k v s.
  (AtLens' map s,
   k ~ Index map,
   v ~ IxValue map,
   Bounded v,
   Eq v)
  => k
  -> Lens' s v
boundedLens k = Dyn.atLens @map k . non (minBound :: v)

-- | A lens to any 'Monoid' instance with default 'mempty'
monoidLens ::
  forall map k v s.
  (Dyn.HasDynamicCache s,
   AtLens' map s,
   k ~ Index map,
   v ~ IxValue map,
   Monoid v,
   Eq v)
  => k
  -> Lens' s v
monoidLens k = Dyn.atLens @map k . non (mempty :: v)

type AtLens' map s =
  (Dyn.HasDynamicCache s,
   At map,
   Typeable map,
   Monoid map,
   Typeable (Index map),
   Ord (Index map),
   Typeable (IxValue map))

-- | Generic 'Map' lens.
class (AnyLens s (Map k v), Ord k) => HasMap k v s where
  mapLens :: HasCallStack => Lens' s (Map k v)
  atLens :: HasCallStack => k -> Lens' s (Maybe v)
  -- ^ Accees an element of a map
  atLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

-- | Generic instance of 'HasMap'.
instance (AnyLens s (Map k v), Ord k, Typeable k, Typeable v) => HasMap k v s where
  mapLens = Dyn.mapLens @(Map k v)
  atLens = Dyn.atLens @(Map k v)
  atLensM k = do
    k' <- k
    pure $ Lens $ atLens k'

ixLens :: forall k v s. HasMap k v s => k -> Traversal' s v
ixLens k = atLens k . _Just

-- | If you don't want to use the 'DynamicCache' declare a 'HasLens'
-- instance.  This is necessary if you want a persistant value
-- (DynamicCache has no Serialize instance) or because you already
-- have a location (not in DynamicCache) where the value is stored.
class HasLens s a where
  hasLens :: Lens' s a

-- runTestTT tests
tests :: Test
tests =
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: Dyn.DynamicCache)
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "test1" (fromList [('a',3),('b',5)]) (view (Dyn.mapLens @(Map Char Int)) m2))
     , TestCase (assertEqual "test2" (Just 5) (view (Dyn.atLens @(Map Char Int) 'b') m2))
     , TestCase (assertEqual "test3" (Just 5) (view (Dyn.mapLens @(Map Char Int) . at 'b') m2))
     , TestCase (assertEqual "test4" Nothing (view (Dyn.atLens @(Map Char Int) 'x') m2))
     , TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (atLens @Char @Int 'x') m2)) ]
