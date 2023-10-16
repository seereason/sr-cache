-- | Similar to dynamic cache but serializable.  This is achieved by
-- encoding the value into a ByteString rather than a Dynamic.  This
-- means higher encoding/decoding overhead, but it also means we can
-- move data between client and server which we cannot with
-- Data.Cache.Dynamic.
--
-- A WARNING though - right now the cache is basically a map from a
-- type to a single value of that type.  There is no way to store more
-- than one value and you can only update the entire value associated
-- with a type.  You can't store more than one value of a given type
-- (though the type could be a map) and you can't query or update less
-- than the entire value associated with a type.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Cache.Encoded
  ( EncodedCache
  , HasEncodedCache(encodedCache)
  , HasEncodedCachePath(encodedCachePath)
  -- , Enc(Enc), enc
    -- * Duplicates for encoded
  , anyLensE
  , maybeLensE
  , mapLensE
  , atLensE
  , atLensME
  , defaultLensE
  , boundedLensE
  , monoidLensE
  , ixLensE
  , anyPathE
  , maybePathE
  , queryEncodedCache
  , queryEncodedCacheOld
  , updateEncodedCache
  , updateEncodedCacheOld
  ) where

import Control.Lens (At(at), Iso', iso, _Just, Lens', non, ReifiedLens(Lens), ReifiedLens', Traversal')
import Control.Lens.Path ((<->), atPath, idPath, nonPath, upcastOptic, PathTo, OpticTag(L), Value(hops), PathError(PathError), UpcastOptic, OpticTag(G), PathToValue(PathToValue))
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.Cache.Common (safeDecode, safeEncode)
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy)
import Data.Serialize (Serialize)
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Stack (HasCallStack)
import SeeReason.Errors as Errors (Member, OneOf, set)

{-
-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype Enc a = Enc a deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

enc :: Iso' (Enc s) s
enc = iso (\(Enc s) -> s) Enc
-}

type EncodedCache = Map Fingerprint ByteString

-- | How to find the encode cache map.
class HasEncodedCache s where
  encodedCache :: Lens' s EncodedCache
instance HasEncodedCache EncodedCache where
  encodedCache = id
class HasEncodedCachePath s where
  encodedCachePath :: PathTo 'L s EncodedCache
instance HasEncodedCachePath EncodedCache where
  encodedCachePath = upcastOptic idPath

deriving instance Serialize Fingerprint

-- Analogues to the Dynamic classes.
class AnyLensE a where
  anyLensE :: HasCallStack => a -> Lens' EncodedCache a
  -- ^ Given a default, build a lens that points to the value of that
  -- type in the 'EncodedCache'

-- | The only instance of 'AnyLensE', builds a lens from
-- 'EncodedCache' to any instance of 'SafeCopy' (which implies
-- 'Typeable')
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
instance SafeCopy a => AnyLensE a where
  anyLensE d =
    {-l0 .-} l1 . l2 . l3
    where
      -- l0 :: Lens' s EncodedCache
      -- l0 = encodedCache @s
      l1 :: Lens' EncodedCache (Maybe ByteString)
      l1 = at (typeRepFingerprint (typeRep (Proxy @a)))
      l2 :: Iso' (Maybe ByteString) ByteString
      l2 = iso (maybe (safeEncode d) id) Just
      l3 :: Iso' ByteString a
      l3 = iso decode' safeEncode
      decode' :: ByteString -> a
      decode' bs = either (\_ -> d) id (safeDecode bs)
  {-# INLINE anyLensE #-}

-- | Generic 'Maybe' lens
class MaybeLensE a where
  maybeLensE :: Lens' EncodedCache (Maybe a)

instance AnyLensE (Maybe a) => MaybeLensE a where
  maybeLensE = anyLensE (Nothing :: Maybe a)

-- | Generic 'Map' lens.
class HasMapE k v where
  mapLensE :: HasCallStack => Lens' EncodedCache (Map k v)
  atLensE :: HasCallStack => k -> Lens' EncodedCache (Maybe v)
  -- ^ Accees an element of a map
  atLensME :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' EncodedCache (Maybe v))

-- | Generic instance of 'HasMapE'.
instance (AnyLensE (Map k v), Ord k, Typeable k, Typeable v) => HasMapE k v where
  mapLensE = anyLensE mempty
  atLensE k = mapLensE @k @v . at k
  atLensME k = do
    k' <- k
    pure $ Lens $ atLensE k'

-- | 'anyLens' for a value with a 'Default' instance.
defaultLensE :: forall a. (AnyLensE a, Default a, HasCallStack) => Lens' EncodedCache a
defaultLensE = anyLensE @a def

-- | 'anyLens' for any value with a 'Bounded' instance.
boundedLensE ::
  forall k v. (AnyLensE (Map k v), Ord k, Typeable k, Typeable v, Bounded v, Eq v, HasCallStack)
  => k -> Lens' EncodedCache v
boundedLensE k = atLensE @k @v k . non (minBound :: v)

-- | 'anyLens' for any value with a 'Monoid' instance.
monoidLensE ::
  forall k v. (AnyLensE (Map k v), Ord k, Typeable k, Typeable v, Monoid v, Eq v, HasCallStack)
  => k -> Lens' EncodedCache v
monoidLensE k = atLensE @k @v k . non (mempty :: v)

ixLensE :: forall k v. (AnyLensE (Map k v), Ord k, Typeable k, Typeable v, HasCallStack) => k -> Traversal' EncodedCache v
ixLensE k = atLensE k . _Just

-- | It would be great to have a path that could go from ByteString to
-- the decoded type a, but at the moment we don't.  So this stops at
-- the 'ByteString'.
anyPathE ::
  forall a. ({-AnyLensE a, Value a, Typeable a,-} SafeCopy a, HasCallStack)
  => a
  -> PathTo 'L EncodedCache ByteString
anyPathE d = atPath (typeRepFingerprint (typeRep (Proxy @a))) <-> nonPath (safeEncode d)

maybePathE ::
  forall a. ({-AnyLensE a, Value a, Typeable a,-} SafeCopy a, HasCallStack)
  => PathTo 'L EncodedCache (Maybe ByteString)
maybePathE = atPath (typeRepFingerprint (typeRep (Proxy @a)))

-- | Retrieve the encoded cache content for type a from the server.
queryEncodedCache ::
  forall db a h e.
  (Monad h,
   -- EventHandler h,
   HasEncodedCachePath db,
   Member PathError e,
   MonadError (OneOf e) h,
   SafeCopy a,
   HasCallStack)
  => (forall o b. (UpcastOptic 'G o, Value b) => PathTo o db b -> h b)
  -> h (Maybe a)
queryEncodedCache queryDatumByGetter =
  queryDatumByGetter (encodedCachePath @db <-> maybePathE @a) >>= \case
    Nothing -> pure Nothing
    Just bs ->
      case safeDecode bs of
        Left s -> throwError (Errors.set (PathError s))
        Right m -> pure m

queryEncodedCacheOld ::
  forall db a h e.
  (Monad h,
   -- EventHandler h,
   HasEncodedCachePath db,
   Member PathError e,
   SafeCopy a,
   HasCallStack)
  => (forall o b. (UpcastOptic 'G o, Value b) => PathTo o db b -> h b)
  -> h (Either (OneOf e) (Maybe a))
queryEncodedCacheOld queryDatumByGetter =
  queryDatumByGetter (encodedCachePath @db <-> maybePathE @a) >>= \case
    Nothing -> pure (Right Nothing)
    Just bs ->
      case safeDecode bs of
        Left s -> pure (Left (Errors.set (PathError s)))
        Right m -> pure (Right m)

-- | Send the encoded cache content for type a to the server.
updateEncodedCache ::
  forall db a h e.
  (HasEncodedCachePath db,
   SafeCopy a,
   HasCallStack)
  => (forall b. Value b => PathToValue 'L db b -> h ())
  -> Maybe a
  -> h ()
updateEncodedCache updateDatumByLens m =
  updateDatumByLens (PathToValue (encodedCachePath @db <-> maybePathE @a) (fmap safeEncode m))

updateEncodedCacheOld ::
  forall db a h e.
  (HasEncodedCachePath db,
   SafeCopy a,
   HasCallStack)
  => (forall b. Value b => PathToValue 'L db b -> h (Maybe (OneOf e)))
  -> Maybe a
  -> h (Maybe (OneOf e))
updateEncodedCacheOld updateDatumByLens m =
  updateDatumByLens (PathToValue (encodedCachePath @db <-> maybePathE @a) (fmap safeEncode m))
