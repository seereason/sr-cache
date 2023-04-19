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
  ( Dyn(Dyn)
  , HasDynamicCache(dynamicCache)
  , Enc(Enc)
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

import Control.Lens ((.=), at, iso, Lens', set, use, view)
import Control.Monad.RWS (evalRWS, RWS, tell)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString (ByteString)
import Data.Cache.Common
import Data.Cache.Dynamic as Dyn (Dyn(Dyn), HasDynamicCache(dynamicCache))
import Data.Cache.Encoded as Enc (Enc(Enc), HasEncodedCache(encodedCache))
import Data.Dynamic (Dynamic)
import Data.Generics.Product
import Data.Generics.Labels ()
import Data.Map (fromList, Map)
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

data Foo = Foo {cache :: Map SomeTypeRep Dynamic} deriving Generic

instance HasDynamicCache Foo where dynamicCache = #cache
instance Typeable a => AnyLens Foo a where anyLens a = dynamicCache . anyLens a

-- runTestTT tests
dynTests :: Test
dynTests =
  TestList $ snd $ evalRWS go () (Foo {cache = mempty})
  where
    go :: RWS () [Test] Foo ()
    go = do
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
{-
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (Dyn (mempty :: Map SomeTypeRep Dynamic))
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [
-}

-- runTestTT tests
encTests :: Test
encTests =
  let m = set (mapLens @Char @Int) (fromList [('a',3),('b',5)] :: Map Char Int) (Enc (mempty :: Map Fingerprint ByteString))
      m2 = set (mapLens @Int @Char) (fromList [(4,'a'),(7,'b')] :: Map Int Char) m
  in TestList
     [ TestCase (assertEqual "test1" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "test2" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "test3" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "test4" Nothing (view (atLens @Char @Int 'x') m2))
     , TestCase (assertEqual "a" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "b" (fromList [('a',3),('b',5)]) (view (mapLens @Char @Int) m2))
     , TestCase (assertEqual "c" (Just 5) (view (atLens @Char @Int 'b') m2))
     , TestCase (assertEqual "d" (Just 5) (view (mapLens @Char @Int . at 'b') m2))
     , TestCase (assertEqual "e" Nothing (view (atLens @Char @Int 'x') m2)) ]
