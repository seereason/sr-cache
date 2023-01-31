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
  ( DynamicCache
  , HasDynamicCache(dynamicCache)
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
  , HasCache(cacheLens, valueLens, valueLensM)
  -- * Tests
  , tests
  ) where

import Control.Lens (At(at), Index, IxValue, Iso', iso, _Just, Lens', non,
                     ReifiedLens', ReifiedLens(Lens), set, Traversal', view)
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

-- | A map from a type fingerprint ('SomeTypeRep') to a wrapped value ('Dynamic') of that type.
newtype DynamicCache = DynamicCache (Map SomeTypeRep Dynamic) deriving (Generic, Monoid, Semigroup)

-- | How to find the 'DynamicCache' value.
class HasDynamicCache a where
  dynamicCache :: Lens' a DynamicCache
instance HasDynamicCache DynamicCache where
  dynamicCache = id

-- | Given a default, build a lens that points into 'DynamicCache' to a
-- value of any 'Typeable' @a@.  The value is initially @d@.
dynamicLens ::
  forall a. (Typeable a, HasCallStack)
  => a -> Lens' DynamicCache a
dynamicLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' DynamicCache (Maybe Dynamic)
    l1 = iso (\(DynamicCache x) -> x) DynamicCache . at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep @a))) . fromDynamic) toDyn
{-# INLINE dynamicLens #-}

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: DynamicCache)
-- \'c\'
-- @
anyLens' :: forall a s. (HasDynamicCache s, Typeable a, HasCallStack) => a -> Lens' s a
anyLens' a = dynamicCache @s . dynamicLens a

-- | Generic lens, allows access to a single @a@ inside a value @s2.
-- This and other classes in this module are used to break import
-- cycles by allowing the use of s without actually having its
-- declaration.
class (HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens :: HasCallStack => a -> Lens' s a

-- | The generic instance of 'AnyLens'.
instance (HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens = anyLens'

-- | 'anyLens' for a 'Maybe' value, with default value 'Nothing'.
maybeLens' :: forall a s. (HasDynamicCache s, Typeable a) => Lens' s (Maybe a)
maybeLens' = anyLens' @(Maybe a) @s Nothing

-- | Generic 'Maybe' lens
class MaybeLens s a where
  maybeLens :: Lens' s (Maybe a)

-- | Generic instance of 'AtLens'.
instance (AnyLens s (Maybe a), Typeable a) => MaybeLens s a where
  maybeLens = maybeLens'

-- | Access the whole map that 'atLens' provides element access to:
--
-- @
--     > view (mapLens \@Char \@String) $
--         atLens \'x\' .~ Just "hello" $
--           atLens \'y\' .~ Just "world" $
--             (mempty :: DynamicCache)
--     fromList [(\'x\',"hello"),(\'y\',"world")]
-- @
mapLens' ::
  forall map s.
  (AtLens' map s,
   HasCallStack)
  => Lens' s map
mapLens' = anyLens' mempty

type AtLens' map s =
  (HasDynamicCache s,
   At map,
   Typeable map,
   Monoid map,
   Typeable (Index map),
   Ord (Index map),
   Typeable (IxValue map))

-- | An 'At' lens to an element of a map.
atLens' ::
  forall map k v s.
  (AtLens' map s,
   k ~ Index map,
   v ~ IxValue map,
   HasCallStack)
  => k
  -> Lens' s (Maybe v)
atLens' k = mapLens' @map . at k

-- | Generic 'Map' lens.
class (AnyLens s (Map k v), Ord k) => HasMap k v s where
  mapLens :: HasCallStack => Lens' s (Map k v)
  atLens :: HasCallStack => k -> Lens' s (Maybe v)
  -- ^ Accees an element of a map
  atLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

-- | Generic instance of 'HasMap'.
instance (AnyLens s (Map k v), Ord k, Typeable k, Typeable v) => HasMap k v s where
  mapLens = mapLens' @(Map k v)
  atLens = atLens' @(Map k v)
  atLensM k = do
    k' <- k
    pure $ Lens $ atLens k'

ixLens :: forall k v s. HasMap k v s => k -> Traversal' s v
ixLens k = atLens k . _Just

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (HasDynamicCache s, Typeable a, Default a) => Lens' s a
defaultLens = anyLens' @a @s def

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
boundedLens k = atLens' @map k . non (minBound :: v)

-- | A lens to any 'Monoid' instance with default 'mempty'
monoidLens ::
  forall map k v s.
  (HasDynamicCache s,
   AtLens' map s,
   k ~ Index map,
   v ~ IxValue map,
   Monoid v,
   Eq v)
  => k
  -> Lens' s v
monoidLens k = atLens' @map k . non (mempty :: v)

-- | If you don't want to use the 'DynamicCache' because you already
-- have a place to store location for a type, declare a 'HasLens' instance.
class HasLens s a where
  hasLens :: Lens' s a

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

-- | Trivial HasCache instance.
instance Ord k => HasCache k v (Map k v) where
  cacheLens = id

-- runTestTT tests
tests :: Test
tests =
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: DynamicCache)
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "test1" (fromList [('a',3),('b',5)]) (view (mapLens' @(Map Char Int)) m2))
     , TestCase (assertEqual "test2" (Just 5) (view (atLens' @(Map Char Int) 'b') m2))
     , TestCase (assertEqual "test3" (Just 5) (view (mapLens' @(Map Char Int) . at 'b') m2))
     , TestCase (assertEqual "test4" Nothing (view (atLens' @(Map Char Int) 'x') m2))
     , TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (atLens @Char @Int 'x') m2)) ]
