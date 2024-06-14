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

module Data.Cache.Typeable
  ( TypeableCache
  , TypeableValue
  , HasTypeableCache(typeableCache)
  -- , Dyn(Dyn), dyn
  , anyLensT
  , maybeLensT
  , mapLensT, atLensT
  , defaultLensT
  , boundedLensT
  , monoidLensT
  , ixLensT
  ) where

import Control.Lens (At(at), Iso', iso, _Just, Lens', non, Traversal')
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


type TypeableCache = Map SomeTypeRep Dynamic

-- | This allows the types in the cache to be restricted, which
-- is helps keep track of what might or might not be in there.
class Typeable a => TypeableValue a

-- | How to find the dynamic cache map.
class HasTypeableCache s where
  typeableCache :: Lens' s TypeableCache
instance HasTypeableCache TypeableCache where
  typeableCache = id

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
-- 'HasTypeableCache' instance to a value of any 'Typeable' @a@.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
anyLensT :: forall s a. (TypeableValue a, HasTypeableCache s, HasCallStack) => a -> Lens' s a
anyLensT d =
  l0 . l1 . l2 . l3
  where
    l0 :: Lens' s TypeableCache
    l0 = typeableCache @s
    l1 :: Lens' TypeableCache (Maybe Dynamic)
    l1 = at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe {-(error ("fromDyn @" <> show (typeRep @a)))-} d . fromDynamic) toDyn
    _ = callStack
{-# INLINE anyLensT #-}

-- | Generic 'Maybe' lens
maybeLensT :: (TypeableValue (Maybe a), HasTypeableCache s, HasCallStack) => Lens' s (Maybe a)
maybeLensT = anyLensT (Nothing :: Maybe a)

-- | Generic 'Map' lens.
mapLensT :: forall k v s. (TypeableValue (Map k v), HasTypeableCache s, Ord k, HasCallStack) => Lens' s (Map k v)
mapLensT = anyLensT mempty

-- | Access an element of a map
atLensT ::forall k v s. (TypeableValue (Map k v), HasTypeableCache s, Ord k, HasCallStack) => k -> Lens' s (Maybe v)
atLensT k = mapLensT @k @v . at k

-- | Generic instance of 'HasMap'.
-- instance (AnyLens s (Map k v), Ord k, Typeable k, Typeable v) => HasMap k v s where

-- | 'anyLens' for a value with a 'Default' instance.
defaultLensT :: forall a s. (TypeableValue a, HasTypeableCache s, Default a, HasCallStack) => Lens' s a
defaultLensT = anyLensT @s @a def

boundedLensT ::
  forall k v s. (TypeableValue (Map k v), HasTypeableCache s, Ord k, Bounded v, Eq v, HasCallStack)
  => k -> Lens' s v
boundedLensT k = atLensT @k @v k . non (minBound :: v)

monoidLensT ::
  forall k v s. (TypeableValue (Map k v), HasTypeableCache s, Ord k, Monoid v, Eq v, HasCallStack)
  => k -> Lens' s v
monoidLensT k = atLensT @k @v k . non (mempty :: v)

ixLensT :: forall k v s. (TypeableValue (Map k v), HasTypeableCache s, Ord k, HasCallStack) => k -> Traversal' s v
ixLensT k = atLensT k . _Just
