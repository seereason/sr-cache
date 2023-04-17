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

module Data.Cache.Dynamic
  ( DynamicCache
  , HasDynamicCache(dynamicCache)
  , anyLens
  , maybeLens
  , atLens
  , mapLens
  , boundedLens
  , defaultLens
  , monoidLens
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
anyLens :: forall a s. (HasDynamicCache s, Typeable a, HasCallStack) => a -> Lens' s a
anyLens a = dynamicCache @s . dynamicLens a

-- | 'anyLens' for a 'Maybe' value, with default value 'Nothing'.
maybeLens :: forall a s. (HasDynamicCache s, Typeable a) => Lens' s (Maybe a)
maybeLens = anyLens @(Maybe a) @s Nothing

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (HasDynamicCache s, Typeable a, Default a) => Lens' s a
defaultLens = anyLens @a @s def

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
boundedLens k = atLens @map k . non (minBound :: v)

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
monoidLens k = atLens @map k . non (mempty :: v)

-- | An 'At' lens to an element of a map.
atLens ::
  forall map k v s.
  (AtLens' map s,
   k ~ Index map,
   v ~ IxValue map,
   HasCallStack)
  => k
  -> Lens' s (Maybe v)
atLens k = mapLens @map . at k

type AtLens' map s =
  (HasDynamicCache s,
   At map,
   Typeable map,
   Monoid map,
   Typeable (Index map),
   Ord (Index map),
   Typeable (IxValue map))

-- | Use 'anylens'' to access a Map.
--
-- @
--     > view (mapLens \@Char \@String) $
--         atLens \'x\' .~ Just "hello" $
--           atLens \'y\' .~ Just "world" $
--             (mempty :: DynamicCache)
--     fromList [(\'x\',"hello"),(\'y\',"world")]
-- @
mapLens ::
  forall map s.
  (AtLens' map s,
   HasCallStack)
  => Lens' s map
mapLens = anyLens mempty
