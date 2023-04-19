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
    HasEncodedCache(encodedCache)
  -- , Enc(Enc), enc
    -- * Duplicates for encoded
  , anyLensE
  , maybeLensE
  , atLensE
  , defaultLensE
  , boundedLensE
  , monoidLensE
  , ixLensE
  ) where

import Control.Lens (At(at), Iso', iso, _Just, Lens', non, ReifiedLens(Lens), ReifiedLens', Traversal')
import Data.ByteString (ByteString)
import Data.Cache.Common
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy)
import Data.Serialize (decode, encode, Serialize)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))

{-
-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype Enc a = Enc a deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

enc :: Iso' (Enc s) s
enc = iso (\(Enc s) -> s) Enc
-}

-- | How to find the encode cache map.
class HasEncodedCache s where
  encodedCache :: Lens' s (Map Fingerprint ByteString)

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
instance (Serialize a, SafeCopy a, HasEncodedCache s) => AnyLensE s a where
  anyLensE d =
    l0 . l1 . l2 . l3
    where
      l0 :: Lens' s (Map Fingerprint ByteString)
      l0 = encodedCache @s
      l1 :: Lens' (Map Fingerprint ByteString) (Maybe ByteString)
      l1 = at (typeRepFingerprint (typeRep (Proxy @a)))
      l2 :: Iso' (Maybe ByteString) ByteString
      l2 = iso (maybe (encode d) id) Just
      l3 :: Iso' ByteString a
      l3 = iso decode' encode
      decode' :: ByteString -> a
      decode' bs = either (error ("decode @" <> show (typeRep (Proxy @a)))) id (decode bs)
  {-# INLINE anyLensE #-}

-- Duplicates of the Dynamic classes.
class AnyLensE s a where
  anyLensE :: HasCallStack => a -> Lens' s a

-- | Generic 'Maybe' lens
class MaybeLensE s a where
  maybeLensE :: Lens' s (Maybe a)

-- | Generic instance of 'AtLens'.
instance AnyLensE s (Maybe a) => MaybeLensE s a where
  maybeLensE = anyLensE (Nothing :: Maybe a)

-- | Generic 'Map' lens.
class HasMapE k v s where
  mapLensE :: HasCallStack => Lens' s (Map k v)
  atLensE :: HasCallStack => k -> Lens' s (Maybe v)
  -- ^ Accees an element of a map
  atLensME :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

-- | Generic instance of 'HasMapE'.
instance (AnyLensE s (Map k v), Ord k, Typeable k, Typeable v) => HasMapE k v s where
  mapLensE = anyLensE mempty
  atLensE k = mapLensE @k @v . at k
  atLensME k = do
    k' <- k
    pure $ Lens $ atLensE k'

-- | 'anyLens' for a value with a 'Default' instance.
defaultLensE :: forall a s. (AnyLensE s a, Default a, HasCallStack) => Lens' s a
defaultLensE = anyLensE @s @a def

boundedLensE ::
  forall k v s. (AnyLensE s (Map k v), Ord k, Typeable k, Typeable v, Bounded v, Eq v, HasCallStack)
  => k -> Lens' s v
boundedLensE k = atLensE @k @v k . non (minBound :: v)

monoidLensE ::
  forall k v s. (AnyLensE s (Map k v), Ord k, Typeable k, Typeable v, Monoid v, Eq v, HasCallStack)
  => k -> Lens' s v
monoidLensE k = atLensE @k @v k . non (mempty :: v)

ixLensE :: forall k v s. (AnyLensE s (Map k v), Ord k, Typeable k, Typeable v, HasCallStack) => k -> Traversal' s v
ixLensE k = atLensE k . _Just
