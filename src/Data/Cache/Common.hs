{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Cache.Common
  ( safeEncode
  , safeDecode
  ) where

import Control.Lens (at, _Just, Lens', non, ReifiedLens(Lens), ReifiedLens', Traversal')
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (runPut, runGet, Serialize)
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.Fingerprint (Fingerprint(..))

-- | Serialize a value using a type's SafeCopy instance.
safeEncode :: SafeCopy a => a -> ByteString
safeEncode = runPut . safePut
safeDecode :: SafeCopy a => ByteString -> Either String a
safeDecode = runGet safeGet

{-
instance Serialize Fingerprint where
  get = do
    a <- get
    b <- get
    pure $ Fingerprint a b
  put (Fingerprint a b) = put a >> put b
-}

#if !MIN_VERSION_base(4,16,0)
deriving instance Generic Fingerprint
#endif
deriving instance Data Fingerprint
instance SafeCopy Fingerprint
deriving instance Serialize Fingerprint
