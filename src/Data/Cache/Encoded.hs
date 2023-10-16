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
{-# LANGUAGE OverloadedLabels #-}
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
  , mapLensE
  , atLensE
  , defaultLensE
  , boundedLensE
  , monoidLensE
  , ixLensE
  , mapPathE
  , atPathE
  , queryEncodedMap
  , updateEncodedMap
  , queryEncodedAt
  , updateEncodedAt
  ) where

import Control.Exception (ErrorCall)
import Control.Monad.Catch (MonadCatch, try)
import Control.Lens (At(at), Iso', iso, _Just, Lens', non, Traversal')
import Control.Lens.Path ((<->), atPath, HopType(NewtypeType), idPath, newtypePath, nonPath, upcastOptic, PathTo, OpticTag(L), Value(hops), PathError(PathError), UpcastOptic, OpticTag(G), PathToValue(PathToValue))
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.Cache.Common (safeDecode, safeEncode)
import Data.Default (Default(def))
import Data.Either (fromRight)
import Data.Generics.Labels ()
import Data.Map.Strict (Map, mapKeys)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy)
import Data.Serialize (Serialize)
import Data.Typeable (typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import SeeReason.Errors as Errors (Member, OneOf, set)

{-
-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype Enc a = Enc a deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

enc :: Iso' (Enc s) s
enc = iso (\(Enc s) -> s) Enc
-}

newtype EncodedCache =
  EncodedCache {unEncodedCache :: Map Fingerprint (Map ByteString ByteString)}
  deriving (Generic, Serialize, SafeCopy, Show, Eq, Ord, Semigroup, Monoid)

instance Value EncodedCache where hops _ = [NewtypeType]

-- | How to find the encode cache map.
class HasEncodedCache s where
  encodedCache :: Lens' s EncodedCache
instance HasEncodedCache EncodedCache where
  encodedCache = id
class HasEncodedCachePath s where
  encodedCachePath :: PathTo 'L s EncodedCache
instance HasEncodedCachePath EncodedCache where
  encodedCachePath = upcastOptic idPath

-- Analogues to the Dynamic classes.
class AtLensE k v where
  mapLensE :: HasCallStack => Lens' EncodedCache (Map k v)
  atLensE :: HasCallStack => k -> Lens' EncodedCache (Maybe v)
  -- ^ Given a default, build a lens that points to the value of that
  -- type in the 'EncodedCache'

-- | The only instance of 'AtLensE', builds a lens from
-- 'EncodedCache' to any instance of 'SafeCopy' (which implies
-- 'Typeable')
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
instance (Ord k, SafeCopy k, SafeCopy v) => AtLensE k v where
  mapLensE =
    l1 . l2
    where
      l1 :: Lens' EncodedCache (Map ByteString ByteString)
      l1 = #unEncodedCache . at (typeRepFingerprint (typeRep (Proxy @(Map k v)))) . non mempty
      l2 :: Iso' (Map ByteString ByteString) (Map k v)
      l2 = iso decodeMap encodeMap
  atLensE k =
    l1 . l2 . l3
    where
      l1 :: Lens' EncodedCache (Map ByteString ByteString)
      l1 = #unEncodedCache . at (typeRepFingerprint (typeRep (Proxy @(Map k v)))) . non mempty
      l2 :: Lens' (Map ByteString ByteString) (Maybe ByteString)
      l2 = at (safeEncode k)
      l3 :: Iso' (Maybe ByteString) (Maybe v)
      l3 = iso decode' (fmap safeEncode)
      decode' :: Maybe ByteString -> Maybe v
      decode' (Just bs) = either (\_ -> Nothing {- This should not happen -}) Just (safeDecode bs)
      decode' Nothing = Nothing
  {-# INLINE atLensE #-}

{-
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
-}

-- | 'anyLens' for a value with a 'Default' instance.
defaultLensE :: forall k v. (AtLensE k v, Eq v, Default v, HasCallStack) => k -> Lens' EncodedCache v
defaultLensE k = atLensE @k @v k . non def

-- | 'anyLens' for any value with a 'Bounded' instance.
boundedLensE ::
  forall k v. (AtLensE k v, Bounded v, Eq v, HasCallStack)
  => k -> Lens' EncodedCache v
boundedLensE k = atLensE @k @v k . non (minBound :: v)

-- | 'anyLens' for any value with a 'Monoid' instance.
monoidLensE ::
  forall k v. (AtLensE k v, Monoid v, Eq v, HasCallStack)
  => k -> Lens' EncodedCache v
monoidLensE k = atLensE @k @v k . non (mempty :: v)

ixLensE :: forall k v. (AtLensE k v, HasCallStack) => k -> Traversal' EncodedCache v
ixLensE k = atLensE k . _Just

mapPathE ::
  forall k v. (SafeCopy k, SafeCopy v, HasCallStack)
  => PathTo 'L EncodedCache (Map ByteString ByteString)
mapPathE =
  newtypePath <->
  atPath (typeRepFingerprint (typeRep (Proxy @(Map k v)))) <->
  nonPath mempty

-- | It would be great to have a path that could go from ByteString to
-- the decoded type a, but at the moment we don't.  So this stops at
-- the 'ByteString'.
atPathE ::
  forall v k. (SafeCopy k, SafeCopy v, HasCallStack)
  => k
  -> PathTo 'L EncodedCache (Maybe ByteString)
atPathE k = mapPathE @k @v <-> atPath (safeEncode k)

-- | Retrieve the encoded cache content for type a from the server.
queryEncodedMap ::
  forall db k v h e.
  (MonadCatch h,
   HasEncodedCachePath db,
   Member PathError e,
   MonadError (OneOf e) h,
   Ord k,
   SafeCopy k,
   SafeCopy v,
   HasCallStack)
  => (forall o b. (UpcastOptic 'G o, Value b) => PathTo o db b -> h b)
  -> h (Map k v)
queryEncodedMap queryDatumByGetter =
  queryDatumByGetter (encodedCachePath @db <-> mapPathE @k @v) >>= \bs ->
    try (pure $ decodeMap bs) >>= \case
      Left (e :: ErrorCall) -> throwError (Errors.set (PathError (show e)))
      Right m -> pure m

decodeMap :: (Ord k, SafeCopy k, SafeCopy v) => Map ByteString ByteString -> Map k v
decodeMap =
  fmap (fromRight (error "decode error") . safeDecode) .
  mapKeys (fromRight (error "decode error") . safeDecode)

encodeMap :: (SafeCopy k, SafeCopy v) => Map k v -> Map ByteString ByteString
encodeMap = fmap safeEncode . mapKeys safeEncode

-- | Send the encoded cache content for type a to the server.
updateEncodedMap ::
  forall db k v h.
  (HasEncodedCachePath db,
   SafeCopy k,
   SafeCopy v,
   HasCallStack)
  => (forall b. Value b => PathToValue 'L db b -> h ())
  -> Map k v
  -> h ()
updateEncodedMap updateDatumByLens m =
  updateDatumByLens (PathToValue
                      (encodedCachePath @db <-> mapPathE @k @v)
                      (fmap safeEncode (mapKeys safeEncode m)))

-- | Retrieve the encoded cache content for type a from the server.
queryEncodedAt ::
  forall db k v h e.
  (Monad h,
   -- EventHandler h,
   HasEncodedCachePath db,
   Member PathError e,
   MonadError (OneOf e) h,
   SafeCopy k,
   SafeCopy v,
   HasCallStack)
  => (forall o b. (UpcastOptic 'G o, Value b) => PathTo o db b -> h b)
  -> k
  -> h (Maybe v)
queryEncodedAt queryDatumByGetter k =
  queryDatumByGetter (encodedCachePath @db <-> atPathE @v k) >>= \case
    Nothing -> pure Nothing
    Just bs ->
      case safeDecode bs of
        Left s -> throwError (Errors.set (PathError s))
        Right m -> pure m

-- | Send the encoded cache content for type a to the server.
updateEncodedAt ::
  forall db k v h.
  (HasEncodedCachePath db,
   SafeCopy k,
   SafeCopy v,
   HasCallStack)
  => (forall b. Value b => PathToValue 'L db b -> h ())
  -> k
  -> Maybe v
  -> h ()
updateEncodedAt updateDatumByLens k m =
  updateDatumByLens (PathToValue (encodedCachePath @db <-> atPathE @v k) (fmap safeEncode m))
