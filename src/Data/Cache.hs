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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Cache
  ( HasDynamicCache(dynamicCache)
  , HasEncodedCache(encodedCache)
  -- , Dyn(Dyn), Enc(Enc)
  -- * For Dynamic
  , anyLens
  , maybeLens
  , mapLens
  , atLens
  , atLensM
  , ixLens
  , defaultLens
  , boundedLens
  , monoidLens
  -- * Encoded duplicates
  , anyLensE
  , maybeLensE
  , atLensE
  , defaultLensE
  , boundedLensE
  , monoidLensE
  , ixLensE
  -- * Non-generic (overridable) lens classes
  , HasLens(hasLens)
  -- * Tests
  , tests
  ) where

import Control.Lens ((.=), at, Lens', set, use, view)
import Control.Monad.RWS (evalRWS, RWS, tell)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString (ByteString)
import Data.Cache.Common
import Data.Cache.Dynamic as Dyn
import Data.Cache.Encoded as Enc
import Data.Dynamic (Dynamic)
import Data.Generics.Labels ()
import Data.Map (fromList, Map)
import Data.SafeCopy (SafeCopy)
import Data.Serialize (Serialize)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics
import Test.HUnit
import Type.Reflection (SomeTypeRep, Typeable)

-- | If you don't want to use the 'Dyn' declare a 'HasLens'
-- instance.  This is necessary if you want a persistant value
-- (Dyn has no Serialize instance) or because you already
-- have a location (not in Dyn) where the value is stored.
class HasLens s a where
  hasLens :: Lens' s a

-- runTestTT tests
tests :: Test
tests = TestList [dynTests, encTests]

data Foo = Foo {dcache :: Map SomeTypeRep Dynamic,
                ecache :: Map Fingerprint ByteString} deriving Generic

instance HasDynamicCache Foo where dynamicCache = #dcache
-- instance HasDynamicCache (Dyn Foo) where dynamicCache = dyn . dynamicCache

instance HasEncodedCache Foo where encodedCache = #ecache
-- instance HasEncodedCache (Enc Foo ) where encodedCache = enc . encodedCache

-- runTestTT tests
dynTests :: Test
dynTests =
  TestList $ snd $ evalRWS cacheTests () (Foo {dcache = mempty, ecache = mempty})
  where

cacheTests :: s ~ Foo => RWS () [Test] s ()
cacheTests = do
  mapLens .= (fromList [('a',3),('b',5)] :: Map Char Int)
  mapLens .= (fromList [(4,'a'),(7,'b')] :: Map Int Char)
  (tellAE "test0" 3.1416) =<< use (anyLens (3.1416 :: Double))
  (tellAE "test0b" (Nothing :: Maybe Float)) =<< use (maybeLens @_ @Float)
  maybeLens .= Just (3.1416 :: Float)
  (tellAE "test0c" (Just 3.1416 :: Maybe Float)) =<< use (maybeLens @_ @Float)
  (tellAE "test1" (fromList [('a',3),('b',5)])) =<< use (mapLens @Char @Int)
  (tellAE "test2" (Just 5)) =<< use (atLens @Char @Int 'b')
  (tellAE "test3" (Just 5)) =<< use (mapLens @Char @Int . at 'b')
  (tellAE "test4" Nothing) =<< use (atLens @Char @Int 'x')
  (tellAE "a" (fromList [('a',3),('b',5)])) =<< use (mapLens @Char @Int)
  (tellAE "b" (fromList [('a',3),('b',5)])) =<< use (mapLens @Char @Int)
  (tellAE "c" (Just 5)) =<< use (atLens @Char @Int 'b')
  (tellAE "d" (Just 5)) =<< use (mapLens @Char @Int . at 'b')
  (tellAE "e" Nothing) =<< use (atLens @Char @Int 'x')

cacheTestsE :: s ~ Foo => RWS () [Test] s ()
cacheTestsE = do
  mapLens .= (fromList [('a',3),('b',5)] :: Map Char Int)
  mapLens .= (fromList [(4,'a'),(7,'b')] :: Map Int Char)
  (tellAE "test0" 3.1416) =<< use (anyLens (3.1416 :: Double))
  (tellAE "test0b" (Nothing :: Maybe Float)) =<< use (maybeLens @_ @Float)
  maybeLens .= Just (3.1416 :: Float)
  (tellAE "test0c" (Just 3.1416 :: Maybe Float)) =<< use (maybeLens @_ @Float)
  (tellAE "test1" (fromList [('a',3),('b',5)])) =<< use (mapLens @Char @Int)
  (tellAE "test2" (Just 5)) =<< use (atLens @Char @Int 'b')
  (tellAE "test3" (Just 5)) =<< use (mapLens @Char @Int . at 'b')
  (tellAE "test4" Nothing) =<< use (atLens @Char @Int 'x')
  (tellAE "a" (fromList [('a',3),('b',5)])) =<< use (mapLens @Char @Int)
  (tellAE "b" (fromList [('a',3),('b',5)])) =<< use (mapLens @Char @Int)
  (tellAE "c" (Just 5)) =<< use (atLens @Char @Int 'b')
  (tellAE "d" (Just 5)) =<< use (mapLens @Char @Int . at 'b')
  (tellAE "e" Nothing) =<< use (atLens @Char @Int 'x')

tellAE :: (MonadWriter [Test] m, Eq a, Show a) => String -> a -> a -> m ()
tellAE name expected value = (tell . (: []) . TestCase . assertEqual name expected) value

-- runTestTT tests
encTests :: Test
encTests =
  TestList $ snd $ evalRWS cacheTestsE () (Foo {dcache = mempty, ecache = mempty})
