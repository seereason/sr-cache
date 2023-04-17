{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Cache.Encoded
  ( -- * Cache type
    EncodedCache
  , HasEncodedCache(encodedCache)
  , tests
  ) where

import Control.Lens (At(at), Iso', iso, Lens', set, view)
import Data.ByteString (ByteString)
import Data.Cache.Common
import Data.Map.Strict (fromList, Map)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy)
import Data.Serialize (decode, encode, Serialize)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import Test.HUnit
import Type.Reflection ()

-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype EncodedCache = EncodedCache (Map Fingerprint ByteString)
  deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

instance SafeCopy EncodedCache

-- | How to find the 'EncodedCache' value.
class HasEncodedCache a where
  encodedCache :: Lens' a EncodedCache
instance HasEncodedCache EncodedCache where
  encodedCache = id

-- | Given a default, build a lens that points into 'EncodedCache' to a
-- value of any 'Typeable' @a@.  The value is initially @d@.
encodedLens ::
  forall a. (Typeable a, Serialize a, HasCallStack)
  => a -> Lens' EncodedCache a
encodedLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' EncodedCache (Maybe ByteString)
    l1 = iso (\(EncodedCache x) -> x) EncodedCache . at (typeRepFingerprint (typeRep (Proxy @a)))
    l2 :: Iso' (Maybe ByteString) ByteString
    l2 = iso (maybe (encode d) id) Just
    l3 :: Iso' ByteString a
    l3 = iso decode' encode
    decode' :: ByteString -> a
    decode' bs = either (error ("decode @" <> show (typeRep (Proxy @a)))) id (decode bs)
{-# INLINE encodedLens #-}

instance (Serialize a, SafeCopy a, HasEncodedCache s) => AnyLens s a where
  anyLens = Data.Cache.Encoded.anyLens

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: DynamicCache)
-- \'c\'
-- @
anyLens :: forall a s. (HasEncodedCache s, Serialize a, SafeCopy a, HasCallStack) => a -> Lens' s a
anyLens a = encodedCache @s . encodedLens a

{-
-- | An 'At' lens to an element of a map.
atLens ::
  forall k v s. (HasMap k v s, Ord k, HasCallStack)
  => k -> Lens' s (Maybe v)
atLens k = Data.Cache.Common.mapLens @k @v . at k

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
  (HasEncodedCache s,
   Monoid map,
   Serialize map,
   SafeCopy map,
   HasCallStack)
  => Lens' s map
mapLens = Data.Cache.Encoded.anyLens mempty
-}

-- runTestTT tests
tests :: Test
tests =
  let m = set (Data.Cache.Common.mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (mempty :: EncodedCache)
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
