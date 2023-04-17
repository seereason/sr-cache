-- | Use Dynamic to create a cache that requires no declarations - you
-- can write @atLens key .= Just value@ and it just works.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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

module Data.Cache
  ( DynamicCache(Dyn)
  , HasDynamicCache(dynamicCache)
  , EncodedCache(Enc)
  , HasEncodedCache(encodedCache)
  , anyLens
  , maybeLens
  , mapLens
  , atLens
  , atLensM
  , ixLens
  , defaultLens
  , boundedLens
  , monoidLens
  -- * Non-generic (overridable) lens classes
  , HasLens(hasLens)
  -- * Tests
  , tests
  ) where

import Control.Lens (at, Lens', set, view)
import Data.ByteString (ByteString)
import Data.Cache.Common
import Data.Cache.Dynamic
import Data.Cache.Encoded as Enc
import Data.Dynamic (Dynamic)
import Data.Map (fromList, Map)
import GHC.Fingerprint (Fingerprint(..))
import Test.HUnit
import Type.Reflection (SomeTypeRep)

-- | If you don't want to use the 'DynamicCache' declare a 'HasLens'
-- instance.  This is necessary if you want a persistant value
-- (DynamicCache has no Serialize instance) or because you already
-- have a location (not in DynamicCache) where the value is stored.
class HasLens s a where
  hasLens :: Lens' s a

-- runTestTT tests
tests :: Test
tests = TestList [dynTests{-, encTests-}]

-- runTestTT tests
dynTests :: Test
dynTests =
  let m = set (Data.Cache.Common.mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (Dyn (mempty :: Map SomeTypeRep Dynamic))
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

#if 0
-- runTestTT tests
encTests :: Test
encTests =
  let m = set (Data.Cache.Common.mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (Enc (mempty :: Map Fingerprint ByteString))
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
#endif
