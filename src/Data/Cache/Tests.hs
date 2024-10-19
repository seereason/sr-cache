{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cache.Tests (tests) where

import Control.Lens ((.=), at, non, use)
import Control.Monad.RWS (evalRWS, RWS, tell)
import Control.Monad.Writer (MonadWriter)
import Data.Generics.Labels ()
import Data.Map (fromList, Map)
import Test.HUnit
import Data.Cache.Common
import Data.Cache.Dynamic as Dyn
import Data.Cache.Encoded as Enc

-- runTestTT tests
tests :: Test
tests = TestList [dynTests, encTests]

-- runTestTT dynTests
dynTests :: Test
dynTests =
  TestList $ snd $ evalRWS cacheTests () (mempty :: DynamicCache)

instance DynamicValue (Map Char Int)
instance DynamicValue (Map Int Char)
instance DynamicValue Double
instance DynamicValue (Maybe Float)
instance EncodedValue Char Int
instance EncodedValue Int Char
instance EncodedValue () Double
instance EncodedValue () Float

cacheTests :: s ~ DynamicCache => RWS () [Test] s ()
cacheTests = do
  Dyn.mapLens .= (fromList [('a',3),('b',5)] :: Map Char Int)
  Dyn.mapLens .= (fromList [(4,'a'),(7,'b')] :: Map Int Char)
  (tellAE "test0" 3.1416) =<< use (Dyn.anyLens (3.1416 :: Double))
  (tellAE "test0b" (Nothing :: Maybe Float)) =<< use (Dyn.maybeLens @_ @Float)
  Dyn.maybeLens .= Just (3.1416 :: Float)
  (tellAE "test0c" (Just 3.1416 :: Maybe Float)) =<< use (Dyn.maybeLens @_ @Float)
  (tellAE "test1" (fromList [('a',3),('b',5)])) =<< use (Dyn.mapLens @Char @Int)
  (tellAE "test2" (Just 5)) =<< use (Dyn.atLens @Char @Int 'b')
  (tellAE "test3" (Just 5)) =<< use (Dyn.mapLens @Char @Int . at 'b')
  (tellAE "test4" Nothing) =<< use (Dyn.atLens @Char @Int 'x')
  (tellAE "a" (fromList [('a',3),('b',5)])) =<< use (Dyn.mapLens @Char @Int)
  (tellAE "b" (fromList [('a',3),('b',5)])) =<< use (Dyn.mapLens @Char @Int)
  (tellAE "c" (Just 5)) =<< use (Dyn.atLens @Char @Int 'b')
  (tellAE "d" (Just 5)) =<< use (Dyn.mapLens @Char @Int . at 'b')
  (tellAE "e" Nothing) =<< use (Dyn.atLens @Char @Int 'x')

cacheTestsE :: s ~ EncodedCache => RWS () [Test] s ()
cacheTestsE = do
  Enc.mapLens .= (fromList [('a',3),('b',5)] :: Map Char Int)
  Enc.mapLens .= (fromList [(4,'a'),(7,'b')] :: Map Int Char)
  (tellAE "test0" 3.1416) =<< use (atLensE () . non (3.1416 :: Double))
  (tellAE "test0b" (Nothing :: Maybe Float)) =<< use (Enc.atLens @_ @Float ())
  Enc.atLens () .= Just (3.1416 :: Float)
  (tellAE "test0c" (Just 3.1416 :: Maybe Float)) =<< use (Enc.atLens @_ @Float ())
  (tellAE "test1" (fromList [('a',3),('b',5)])) =<< use (Enc.mapLens @Char @Int)
  (tellAE "test2" (Just 5)) =<< use (Enc.atLens @Char @Int 'b')
  (tellAE "test3" (Just 5)) =<< use (Enc.mapLens @Char @Int . at 'b')
  (tellAE "test4" Nothing) =<< use (Enc.atLens @Char @Int 'x')
  (tellAE "a" (fromList [('a',3),('b',5)])) =<< use (Enc.mapLens @Char @Int)
  (tellAE "b" (fromList [('a',3),('b',5)])) =<< use (Enc.mapLens @Char @Int)
  (tellAE "c" (Just 5)) =<< use (Enc.atLens @Char @Int 'b')
  (tellAE "d" (Just 5)) =<< use (Enc.mapLens @Char @Int . at 'b')
  (tellAE "e" Nothing) =<< use (Enc.atLens @Char @Int 'x')

tellAE :: (MonadWriter [Test] m, Eq a, Show a) => String -> a -> a -> m ()
tellAE name expected value = (tell . (: []) . TestCase . assertEqual name expected) value

-- runTestTT tests
encTests :: Test
encTests = TestList $ snd $ evalRWS cacheTestsE () mempty
