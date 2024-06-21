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
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Cache.Dynamic
  ( DynamicCache
  , DynamicValue
  , HasDynamicCache(dynamicCache)
  -- , Dyn(Dyn), dyn
  , unsafeDynamicLens
  , anyLens
  , maybeLens
  , mapLens, atLens
  , defaultLens
  , boundedLens
  , monoidLens
  , ixLens
  ) where

import Control.Lens (at, ix, Iso', iso, Lens', Traversal')
-- import Data.Cache.Common
import Data.Default (Default(def))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
-- import GHC.Generics
import GHC.Stack (HasCallStack, callStack)
import Type.Reflection (SomeTypeRep, someTypeRep)


type DynamicCache = Map SomeTypeRep Dynamic

-- | This allows the types in the cache to be restricted, which
-- is helps keep track of what might or might not be in there.
class Typeable a => DynamicValue a

-- | How to find the dynamic cache map.
class HasDynamicCache s where
  dynamicCache :: Lens' s DynamicCache
instance HasDynamicCache DynamicCache where
  dynamicCache = id

{-
-- | A map from a type fingerprint ('SomeTypeRep') to a wrapped value ('Dynamic') of that type.
newtype Dyn s = Dyn s deriving (Generic, Monoid, Semigroup)

dyn :: Iso' (Dyn s) s
dyn = iso (\(Dyn s) -> s) Dyn
-}

-- | Generic lens, allows access to a single @a@ inside a value @s2.
-- This and other classes in this module are used to break import
-- cycles by allowing the use of s without actually having its
-- declaration.
--
-- Given a default, build a lens that points into any
-- 'HasDynamicCache' instance to a value of any 'Typeable' @a@.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
anyLens :: forall s a. (HasDynamicCache s, DynamicValue a, HasCallStack) => a -> Lens' s a
anyLens d =
  dynamicCache @s .  unsafeDynamicLens d
  where
    _ = callStack
{-# INLINE anyLens #-}

unsafeDynamicLens :: forall a. (Typeable a, HasCallStack) => a -> Lens' DynamicCache a
unsafeDynamicLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' DynamicCache (Maybe Dynamic)
    l1 = at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe {-(error ("fromDyn @" <> show (typeRep @a)))-} d . fromDynamic) toDyn
    _ = callStack
{-# INLINE unsafeDynamicLens #-}

maybeLens :: (HasDynamicCache s, DynamicValue (Maybe a), HasCallStack) => Lens' s (Maybe a)
maybeLens = anyLens (Nothing :: Maybe a)

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (HasDynamicCache s, DynamicValue a, Default a, HasCallStack) => Lens' s a
defaultLens = anyLens @s @a def

boundedLens ::
  forall a s. (HasDynamicCache s, DynamicValue a, Bounded a, HasCallStack)
  => Lens' s a
boundedLens = anyLens @s @a (minBound :: a)

monoidLens ::
  forall a s. (HasDynamicCache s, DynamicValue a, Monoid a, HasCallStack)
  => Lens' s a
monoidLens = anyLens @s @a (mempty :: a)

-- | Generic 'Map' lens.
mapLens :: forall k v s. (HasDynamicCache s, DynamicValue (Map k v), Ord k, HasCallStack) => Lens' s (Map k v)
mapLens = anyLens mempty

-- | Access an element of a map
atLens ::forall k v s. (HasDynamicCache s, DynamicValue (Map k v), Ord k, HasCallStack) => k -> Lens' s (Maybe v)
atLens k = mapLens @k @v . at k

ixLens :: forall k v s. (HasDynamicCache s, DynamicValue (Map k v), Ord k, HasCallStack) => k -> Traversal' s v
ixLens k = mapLens @k @v . ix k
