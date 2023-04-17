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

module Data.Cache
  ( Dyn.DynamicCache
  , Dyn.HasDynamicCache(dynamicCache)
  , anyLens
  , maybeLens
  , mapLens
  , atLens
  , atLensM
  , ixLens
  -- * Non-generic (overridable) lens classes
  , HasLens(hasLens)
  -- * Tests
  , tests
  ) where

import Control.Lens (Lens')
import Data.Cache.Common
import qualified Data.Cache.Dynamic as Dyn
import qualified Data.Cache.Encoded as Enc
import Test.HUnit

-- | If you don't want to use the 'DynamicCache' declare a 'HasLens'
-- instance.  This is necessary if you want a persistant value
-- (DynamicCache has no Serialize instance) or because you already
-- have a location (not in DynamicCache) where the value is stored.
class HasLens s a where
  hasLens :: Lens' s a

-- runTestTT tests
tests :: Test
tests = TestList [Dyn.tests, Enc.tests]
