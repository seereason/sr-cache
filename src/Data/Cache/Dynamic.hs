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
  ) where

import Control.Lens (At(at), Iso', iso, Lens')
import Data.Cache.Common
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
-- import GHC.Generics
-- import GHC.Stack (HasCallStack)
import Type.Reflection (SomeTypeRep, someTypeRep, typeRep)

{-
-- | A map from a type fingerprint ('SomeTypeRep') to a wrapped value ('Dynamic') of that type.
newtype Dyn s = Dyn s deriving (Generic, Monoid, Semigroup)

dyn :: Iso' (Dyn s) s
dyn = iso (\(Dyn s) -> s) Dyn
-}

type DynamicCache = Map SomeTypeRep Dynamic

-- | How to find the dynamic cache map.
class HasDynamicCache s where
  dynamicCache :: Lens' s DynamicCache

-- | Given a default, build a lens that points into any
-- 'HasDynamicCache' instance to a value of any 'Typeable' @a@.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
instance (Typeable a, HasDynamicCache s) => AnyLens s a where
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
      l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep @a))) . fromDynamic) toDyn
  {-# INLINE anyLens #-}
