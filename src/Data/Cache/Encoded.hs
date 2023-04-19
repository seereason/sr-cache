{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
    Enc(Enc)
  , HasEncodedCache(encodedCache)
  ) where

import Control.Lens (At(at), Iso', iso, Lens')
import Data.ByteString (ByteString)
import Data.Cache.Common
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy)
import Data.Serialize (decode, encode, Serialize)
import GHC.Generics
-- import GHC.Stack (HasCallStack)
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))

-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype Enc a = Enc a deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

instance SafeCopy a => SafeCopy (Enc a)

-- | How to find the encode cache map.
class HasEncodedCache s where
  encodedCache :: Lens' s (Map Fingerprint ByteString)
instance HasEncodedCache (Enc (Map Fingerprint ByteString)) where
  encodedCache = iso (\(Enc s) -> s) Enc

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
instance (Serialize a, SafeCopy a, HasEncodedCache (Enc s)) => AnyLens (Enc s) a where
  anyLens d =
    l0 . l1 . l2 . l3
    where
      l0 :: Lens' (Enc s) (Map Fingerprint ByteString)
      l0 = encodedCache @(Enc s)
      l1 :: Lens' (Map Fingerprint ByteString) (Maybe ByteString)
      l1 = at (typeRepFingerprint (typeRep (Proxy @a)))
      l2 :: Iso' (Maybe ByteString) ByteString
      l2 = iso (maybe (encode d) id) Just
      l3 :: Iso' ByteString a
      l3 = iso decode' encode
      decode' :: ByteString -> a
      decode' bs = either (error ("decode @" <> show (typeRep (Proxy @a)))) id (decode bs)
  {-# INLINE anyLens #-}

instance (SafeCopy a, Serialize a, Typeable a) => AnyLens (Map Fingerprint ByteString) a where
  anyLens a = iso Enc (\(Enc s) -> s) . anyLens @(Enc (Map Fingerprint ByteString)) a
