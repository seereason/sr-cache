{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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

module Data.GenericCache
  ( -- * Cache type
    GenericCache
  , HasGenericCache(genericCache)
    -- * Lens functions
  , anyLens
  , maybeLens
  , defaultLens
    -- * Map lens functions
  , atLens
  , mapLens
  , ixLens
  , boundedLens
  , monoidLens
    -- * Non-generic lens
  , HasLens(hasLens)
    -- * Tests
  , tests
  ) where

import Control.Lens (At(at), Index, IxValue, Iso', iso, _Just, Lens', non, Traversal')
import Data.Default (Default(def))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Type.Reflection

import Control.Lens (set, view)
import Data.Map.Strict (fromList)
import Test.HUnit

-- | A map from a type fingerprint ('SomeTypeRep') to a wrapped value ('Dynamic') of that type.
newtype GenericCache = GenericCache (Map SomeTypeRep Dynamic) deriving (Generic, Monoid, Semigroup)

-- | How to find the 'GenericCache' value.
class HasGenericCache a where
  genericCache :: Lens' a GenericCache
instance HasGenericCache GenericCache where
  genericCache = id

-- | Given a default, build a lens that points into 'GenericCache' to a
-- value of any 'Typeable' @a@.  The value is initially @d@.
dynamicLens ::
  forall a. (Typeable a, HasCallStack)
  => a -> Lens' GenericCache a
dynamicLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' GenericCache (Maybe Dynamic)
    l1 = iso (\(GenericCache x) -> x) GenericCache . at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep @a))) . fromDynamic) toDyn

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: GenericCache)
-- \'c\'
-- @
anyLens :: forall a s. (HasGenericCache s, Typeable a, HasCallStack) => a -> Lens' s a
anyLens a = genericCache @s . dynamicLens a

-- | 'anyLens' for a 'Maybe' value, with default value 'Nothing'.
maybeLens :: forall a s. (HasGenericCache s, Typeable a) => Lens' s (Maybe a)
maybeLens = anyLens @(Maybe a) @s Nothing

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (HasGenericCache s, Typeable a, Default a) => Lens' s a
defaultLens = anyLens @a @s def

type AtLens map s =
  (HasGenericCache s,
   At map,
   Typeable map,
   Monoid map,
   Typeable (Index map),
   Ord (Index map),
   Typeable (IxValue map))

-- | An 'At' lens to an element of a map.
atLens ::
  forall map k v s.
  (AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   HasCallStack)
  => k
  -> Lens' s (Maybe v)
atLens k = mapLens @map . at k

-- | Access the whole map that 'atLens' provides element access to:
--
-- @
--     > view (mapLens \@Char \@String) $
--         atLens \'x\' .~ Just "hello" $
--           atLens \'y\' .~ Just "world" $
--             (mempty :: GenericCache)
--     fromList [(\'x\',"hello"),(\'y\',"world")]
-- @
mapLens ::
  forall map s.
  (AtLens map s,
   HasCallStack)
  => Lens' s map
mapLens = anyLens mempty

ixLens ::
  forall map k v s.
  (AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   HasCallStack)
  => Index map
  -> Traversal' s v
ixLens k = atLens @map k . _Just

boundedLens ::
  forall map k v s.
  (AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   Bounded v,
   Eq v)
  => k
  -> Lens' s v
boundedLens k = atLens @map k . non (minBound :: v)

monoidLens ::
  forall map k v s.
  (HasGenericCache s,
   AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   Monoid v,
   Eq v)
  => k
  -> Lens' s v
monoidLens k = atLens @map k . non (mempty :: v)

-- | If you don't want to use the 'GenericCache' because you already
-- have a place to store location for a type, declare a 'HasLens' instance.
class HasLens s a where
  hasLens :: Lens' s a

-- runTestTT tests
tests :: Test
tests =
  let m = set (mapLens @(Map Char Int)) (fromList [('a',3),('b',5)]) (mempty :: GenericCache)
      m2 = set (mapLens @(Map Int Char)) (fromList [(4,'a'),(7,'b')]) m
  in TestList
     [ TestCase (assertEqual "test1" (fromList [('a',3),('b',5)]) (view (mapLens @(Map Char Int)) m2))
     , TestCase (assertEqual "test2" (Just 5) (view (atLens @(Map Char Int) 'b') m2))
     , TestCase (assertEqual "test3" (Just 5) (view (mapLens @(Map Char Int) . at 'b') m2))
     , TestCase (assertEqual "test4" Nothing (view (atLens @(Map Char Int) 'x') m2)) ]
