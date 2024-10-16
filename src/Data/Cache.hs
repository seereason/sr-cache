-- | Use Dynamic to create a cache that requires no declarations - you
-- can write @atLens key .= Just value@ and it just works.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Cache
  ( HasDynamicCache(dynamicCache), DynamicCache, DynamicValue
  , HasEncodedCache, EncodedCache
  , HasEncodedCachePath(encodedCachePath)
  -- , Dyn(Dyn), Enc(Enc)
  -- * For Dynamic
  , anyLens
  , maybeLens
  , mapLens
  , atLens
  , ixLens
  , defaultLens
  , boundedLens
  , monoidLens
  -- * Encoded duplicates
  , atLensE
  , defaultLensE
  , boundedLensE
  , monoidLensE
  , ixLensE
  ) where

import Data.Cache.Common ()
import Data.Cache.Dynamic
import Data.Cache.Encoded
