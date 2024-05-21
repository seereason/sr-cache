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
  , HasDynamicCache(dynamicCache)
  -- , Dyn(Dyn), dyn
  , anyLens
  , maybeLens
  , mapLens, atLens, atLensM
  , defaultLens
  , boundedLens
  , monoidLens
  , ixLens
  ) where

import Control.Lens (At(at), Iso', iso, _Just, Lens', non, ReifiedLens(Lens), ReifiedLens', Traversal')
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
anyLens :: forall s a. (HasDynamicCache s, Typeable a, HasCallStack) => a -> Lens' s a
anyLens d =
  l0 . l1 . l2 . l3
  where
    l0 :: Lens' s DynamicCache
    l0 = dynamicCache @s
    l1 :: Lens' DynamicCache (Maybe Dynamic)
    l1 = at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe {-(error ("fromDyn @" <> show (typeRep @a)))-} d . fromDynamic) toDyn
    _ = callStack
{-# INLINE anyLens #-}

-- | Generic 'Maybe' lens
maybeLens :: (HasDynamicCache s, Typeable a, HasCallStack) => Lens' s (Maybe a)
maybeLens = anyLens (Nothing :: Maybe a)

-- | Generic 'Map' lens.
mapLens :: forall k v s. (HasDynamicCache s, Typeable k, Ord k, Typeable v, HasCallStack) => Lens' s (Map k v)
mapLens = anyLens mempty

-- | Access an element of a map
atLens ::forall k v s. (HasDynamicCache s, Typeable k, Ord k, Typeable v, HasCallStack) => k -> Lens' s (Maybe v)
atLens k = mapLens @k @v . at k

atLensM :: forall k v s m. (HasDynamicCache s, Ord k, Typeable k, Typeable v, Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))
atLensM k = do
  k' <- k
  pure $ Lens $ atLens k'

-- | Generic instance of 'HasMap'.
-- instance (AnyLens s (Map k v), Ord k, Typeable k, Typeable v) => HasMap k v s where

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (HasDynamicCache s, Typeable a, Default a, HasCallStack) => Lens' s a
defaultLens = anyLens @s @a def

boundedLens ::
  forall k v s. (HasDynamicCache s, Ord k, Typeable k, Typeable v, Bounded v, Eq v, HasCallStack)
  => k -> Lens' s v
boundedLens k = atLens @k @v k . non (minBound :: v)

monoidLens ::
  forall k v s. (HasDynamicCache s, Ord k, Typeable k, Typeable v, Monoid v, Eq v, HasCallStack)
  => k -> Lens' s v
monoidLens k = atLens @k @v k . non (mempty :: v)

ixLens :: forall k v s. (HasDynamicCache s, Ord k, Typeable k, Typeable v, HasCallStack) => k -> Traversal' s v
ixLens k = atLens k . _Just
