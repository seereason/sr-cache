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
    EncodedCache(Enc)
  , HasEncodedCache(encodedCache)
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
import Data.Typeable (typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import Test.HUnit
import Type.Reflection ()

-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype EncodedCache a = Enc a deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

instance SafeCopy a => SafeCopy (EncodedCache a)

-- | How to find the 'EncodedCache' value.
class HasEncodedCache s where
  encodedCache :: Lens' s (Map Fingerprint ByteString)
instance HasEncodedCache (EncodedCache (Map Fingerprint ByteString)) where
  encodedCache = iso (\(Enc s) -> s) Enc

instance (Serialize a, SafeCopy a, HasEncodedCache (EncodedCache s)) => AnyLens (EncodedCache s) a where
  anyLens = Data.Cache.Encoded.anyLens

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: DynamicCache)
-- \'c\'
-- @
anyLens :: forall a s. (HasEncodedCache s, Serialize a, SafeCopy a, HasCallStack) => a -> Lens' s a
anyLens d =
  encodedCache @s . l1 . l2 . l3
  where
    l1 :: Lens' (Map Fingerprint ByteString) (Maybe ByteString)
    l1 = at (typeRepFingerprint (typeRep (Proxy @a)))
    l2 :: Iso' (Maybe ByteString) ByteString
    l2 = iso (maybe (encode d) id) Just
    l3 :: Iso' ByteString a
    l3 = iso decode' encode
    decode' :: ByteString -> a
    decode' bs = either (error ("decode @" <> show (typeRep (Proxy @a)))) id (decode bs)
{-# INLINE anyLens #-}

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
