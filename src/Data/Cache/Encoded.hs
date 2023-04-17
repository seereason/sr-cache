{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
-- {-# LANGUAGE DeriveAnyClass #-}
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
{-# OPTIONS -Wall #-}

module Data.Cache.Encoded
  ( -- * Cache type
    EncodedCache
  , HasEncodedCache(encodedCache)
  , anyLens
  , maybeLens
  , atLens
  , mapLens
  , boundedLens
  , defaultLens
  , monoidLens
  ) where

import Control.Lens (At(at), Index, IxValue, Iso', iso, _Just, Lens', non, Traversal')
import Data.ByteString (ByteString)
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy)
import Data.Serialize (decode, encode, Serialize(get, put))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics
import GHC.Stack (HasCallStack)
import Type.Reflection ()

instance Serialize Fingerprint where
  get = do
    a <- get
    b <- get
    pure $ Fingerprint a b
  put (Fingerprint a b) = put a >> put b

#if !MIN_VERSION_base(4,16,0)
deriving instance Generic Fingerprint
#endif
instance SafeCopy Fingerprint

-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype EncodedCache = EncodedCache (Map Fingerprint ByteString)
  deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

instance SafeCopy EncodedCache

-- | How to find the 'EncodedCache' value.
class HasEncodedCache a where
  encodedCache :: Lens' a EncodedCache
instance HasEncodedCache EncodedCache where
  encodedCache = id

-- | Given a default, build a lens that points into 'EncodedCache' to a
-- value of any 'Typeable' @a@.  The value is initially @d@.
encodedLens ::
  forall a. (Typeable a, Serialize a, HasCallStack)
  => a -> Lens' EncodedCache a
encodedLens d =
  l1 . l2 . l3
  where
    l1 :: Lens' EncodedCache (Maybe ByteString)
    l1 = iso (\(EncodedCache x) -> x) EncodedCache . at (typeRepFingerprint (typeRep (Proxy @a)))
    l2 :: Iso' (Maybe ByteString) ByteString
    l2 = iso (maybe (encode d) id) Just
    l3 :: Iso' ByteString a
    l3 = iso decode' encode
    decode' :: ByteString -> a
    decode' bs = either (error ("decode @" <> show (typeRep (Proxy @a)))) id (decode bs)

-- | Generic lens, allows access to a single @a@ inside a value @s@.
-- It has a default value argument.
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: DynamicCache)
-- \'c\'
-- @
anyLens :: forall a s. (HasEncodedCache s, Typeable a, Serialize a, SafeCopy a, HasCallStack) => a -> Lens' s a
anyLens a = encodedCache @s . encodedLens a

-- | 'anyLens' for a 'Maybe' value, with default value 'Nothing'.
maybeLens :: forall a s. (HasEncodedCache s, Typeable a, Serialize a, SafeCopy a, HasCallStack) => Lens' s (Maybe a)
maybeLens = anyLens @(Maybe a) @s Nothing

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (Default a, HasEncodedCache s, Typeable a, Serialize a, SafeCopy a, HasCallStack) => Lens' s a
defaultLens = anyLens @a @s def

type AtLens map s =
  (HasEncodedCache s,
   At map,
   Typeable map,
   Monoid map,
   Serialize map,
   SafeCopy map,
   Typeable (Index map),
   Ord (Index map),
   Typeable (IxValue map))

-- | An 'At' lens to an element of a map.
atLens ::
  forall map k v s.
  (AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   HasCallStack)
  => k
  -> Lens' s (Maybe v)
atLens k = mapLens @map . at k

-- | Access the whole map that 'atLens' provides element access to:
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
  (AtLens map s,
   HasCallStack)
  => Lens' s map
mapLens = anyLens mempty

boundedLens ::
  forall map k v s.
  (AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   Bounded v,
   Eq v)
  => k
  -> Lens' s v
boundedLens k = atLens @map k . non (minBound :: v)

monoidLens ::
  forall map k v s.
  (HasEncodedCache s,
   AtLens map s,
   k ~ Index map,
   v ~ IxValue map,
   Monoid v,
   Eq v)
  => k
  -> Lens' s v
monoidLens k = atLens @map k . non (mempty :: v)
