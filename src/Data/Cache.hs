{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Cache
  ( HasDynamicCache(dynamicCache)
  , CacheMaps
  , dynamicLens
  , mayLens
  , anyLens
  , mapLens
  , atLens
  , atLensM
  , tests
  ) where

import Control.Lens (at, Iso', iso, Lens', ReifiedLens', ReifiedLens(Lens))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Stack (HasCallStack)

import Control.Lens (set, view)
import Data.Map.Strict (fromList)
import Test.HUnit

-- | A map from type fingerprint values to wrapped types of that
-- value.  The internals of this ought be hidden to preserve the
-- constraint that this is an Iso between a type and a single value
-- of that type.
type CacheMaps = Map Fingerprint Dynamic

-- | How to find the 'CacheMaps' value.
class HasDynamicCache a where dynamicCache :: Lens' a CacheMaps
instance HasDynamicCache CacheMaps where dynamicCache = id

-- | Given a default, build a lens that points into 'CacheMaps' to a
-- value of any Typeable a.  The value is initially d.
dynamicLens ::
  forall a. (Typeable a, HasCallStack)
  => a -> Lens' CacheMaps a
dynamicLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' CacheMaps (Maybe Dynamic)
    l1 = at (typeRepFingerprint (typeRep (Proxy @a)))
    l2 :: Iso' (Maybe Dynamic) Dynamic
    l2 = iso (maybe (toDyn d) id) Just
    l3 :: Iso' Dynamic a
    l3 = iso (fromMaybe (error ("fromDyn @" <> show (typeRep (Proxy @a)))) . fromDynamic) toDyn

class (HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens :: HasCallStack => a -> Lens' s a

instance (HasDynamicCache s, Typeable a) => AnyLens s a where
  anyLens a = dynamicCache @s . dynamicLens a

class AtLens s a where
  mayLens :: Lens' s (Maybe a)

instance AnyLens s (Maybe a) => AtLens s a where
  mayLens = anyLens @s @(Maybe a) Nothing

class Ord k => HasMap k v s where
  mapLens :: HasCallStack => Lens' s (Map k v)
  atLens :: HasCallStack => k -> Lens' s (Maybe v)
  atLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

instance (HasDynamicCache s, Ord k, Typeable k, Typeable v) => HasMap k v s where
  mapLens = anyLens mempty
  atLens k = mapLens . at k
  atLensM k = do
    k' <- k
    pure $ Lens $ atLens k'

tests :: Test
tests =
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: CacheMaps)
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (atLens @Char @Int 'x') m2)) ]
