{-# LANGUAGE FlexibleContexts #-}
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
import Data.Cache.Dynamic
import Data.Cache.Encoded

-- runTestTT tests
tests :: Test
tests = TestList [dynTests, encTests]

-- runTestTT dynTests
dynTests :: Test
dynTests =
  TestList $ snd $ evalRWS cacheTests () (mempty :: DynamicCache)

cacheTests :: s ~ DynamicCache => RWS () [Test] s ()
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

cacheTestsE :: s ~ EncodedCache => RWS () [Test] s ()
cacheTestsE = do
  mapLensE .= (fromList [('a',3),('b',5)] :: Map Char Int)
  mapLensE .= (fromList [(4,'a'),(7,'b')] :: Map Int Char)
  (tellAE "test0" 3.1416) =<< use (atLensE () . non (3.1416 :: Double))
  (tellAE "test0b" (Nothing :: Maybe Float)) =<< use (atLensE @_ @Float ())
  atLensE () .= Just (3.1416 :: Float)
  (tellAE "test0c" (Just 3.1416 :: Maybe Float)) =<< use (atLensE @_ @Float ())
  (tellAE "test1" (fromList [('a',3),('b',5)])) =<< use (mapLensE @Char @Int)
  (tellAE "test2" (Just 5)) =<< use (atLensE @Char @Int 'b')
  (tellAE "test3" (Just 5)) =<< use (mapLensE @Char @Int . at 'b')
  (tellAE "test4" Nothing) =<< use (atLensE @Char @Int 'x')
  (tellAE "a" (fromList [('a',3),('b',5)])) =<< use (mapLensE @Char @Int)
  (tellAE "b" (fromList [('a',3),('b',5)])) =<< use (mapLensE @Char @Int)
  (tellAE "c" (Just 5)) =<< use (atLensE @Char @Int 'b')
  (tellAE "d" (Just 5)) =<< use (mapLensE @Char @Int . at 'b')
  (tellAE "e" Nothing) =<< use (atLensE @Char @Int 'x')

tellAE :: (MonadWriter [Test] m, Eq a, Show a) => String -> a -> a -> m ()
tellAE name expected value = (tell . (: []) . TestCase . assertEqual name expected) value

-- runTestTT tests
encTests :: Test
encTests = TestList $ snd $ evalRWS cacheTestsE () mempty
