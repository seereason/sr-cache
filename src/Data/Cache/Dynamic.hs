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
  , tests
  ) where

import Control.Lens (At(at), Iso', iso, Lens', ReifiedLens(Lens), set, view)
import Data.Cache.Common
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

-- | The generic instance of 'AnyLens'.
instance (Typeable a, HasDynamicCache s) => AnyLens s a where
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
    l1 :: Lens' DynamicCache (Maybe Dynamic)
    l1 = iso (\(DynamicCache x) -> x) DynamicCache . at (someTypeRep (Proxy @a))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep @a))) . fromDynamic) toDyn
{-# INLINE anyLens #-}

-- runTestTT tests
tests :: Test
tests =
  let m = set (Data.Cache.Common.mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: DynamicCache)
      m2 = set (Data.Cache.Common.mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "test1" (fromList [('a',3),('b',5)]) (view (Data.Cache.Common.mapLens @Char @Int) m2))
     , TestCase (assertEqual "test2" (Just 5) (view (Data.Cache.Common.atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "test3" (Just 5) (view (Data.Cache.Common.mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "test4" Nothing (view (Data.Cache.Common.atLens @Char @Int 'x') m2))
     , TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (Data.Cache.Common.mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (Data.Cache.Common.mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (Data.Cache.Common.atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (Data.Cache.Common.mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (Data.Cache.Common.atLens @Char @Int 'x') m2)) ]
