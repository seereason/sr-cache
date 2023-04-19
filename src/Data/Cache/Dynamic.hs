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
  ( DynamicCache(Dyn)
  , HasDynamicCache(dynamicCache)
  ) where

import Control.Lens (At(at), Iso', iso, Lens')
import Data.Cache.Common
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Type.Reflection

-- | A map from a type fingerprint ('SomeTypeRep') to a wrapped value ('Dynamic') of that type.
newtype DynamicCache s = Dyn s deriving (Generic, Monoid, Semigroup)

-- | How to find the 'DynamicCache' value.
class HasDynamicCache s where
  dynamicCache :: Lens' s (Map SomeTypeRep Dynamic)
instance HasDynamicCache (DynamicCache (Map SomeTypeRep Dynamic)) where
  dynamicCache = iso (\(Dyn s) -> s) Dyn

-- | The generic instance of 'AnyLens'.
instance (Typeable a, HasDynamicCache (DynamicCache s)) => AnyLens (DynamicCache s) a where
  anyLens = Data.Cache.Dynamic.anyLens

-- | Given a default, build a lens that points into any
-- 'HasDynamicCache' instance to a value of any 'Typeable' @a@.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: DynamicCache)
-- \'c\'
-- @
anyLens :: forall a s. (HasDynamicCache s, Typeable a, HasCallStack) => a -> Lens' s a
anyLens d =
  dynamicCache @s . l1 . l2 . l3
  where
    l1 :: Lens' (Map SomeTypeRep Dynamic) (Maybe Dynamic)
    l1 = at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep @a))) . fromDynamic) toDyn
{-# INLINE anyLens #-}
