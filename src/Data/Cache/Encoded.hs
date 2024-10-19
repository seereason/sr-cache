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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Cache.Encoded
  ( EncodedCache(..)
  , EncodedValue
  , EncodedValue'
  , HasEncodedCache(encodedCache)
  , HasEncodedCachePath(encodedCachePath)

  -- * Basic encoded cache lenses
  , mapLensE, mapLensE'
  , atLensE, atLensE'
  , ixLensE, ixLensE'

  , defaultLensE
  , boundedLensE
  , monoidLensE
  , mapPathE
  , atPathE

  -- * Dynamic cache replacements
  , mapLens, atLens, ixLens
  , anyLens, maybeLens

  , queryEncodedMap
  , updateEncodedMap
  , queryEncodedAt
  , updateEncodedAt

  , getEncoded
  , useEncoded
  , askEncoded
  , viewEncoded
  , previewEncoded
  , preuseEncoded
  , putEncoded

  , recvEncoded
  , sendEncoded
  , overEncoded
  ) where

import Control.Exception (ErrorCall)
import Control.Monad.Catch (MonadCatch, try)
import Control.Lens (_1, At(at), Iso', iso, _Just, Lens', non, Traversal', use, view, (.=), (%=), Getter, Fold, preview, preuse)
import Control.Lens.Path
  ((<->), atPath, fstPath, HopType(NewtypeType), idPath, IsGetterTag, newtypePath,
   nonPath, upcastOptic, PathTo, OpticTag(L), Value(hops), PathError(PathError),
   PathToValue(PathToValue))
import Control.Monad.Except (MonadError, MonadIO, throwError, when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.ByteString (ByteString)
import Data.Cache.Common (safeDecode, safeEncode)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.Either (fromRight)
import Data.Generics.Labels ()
import Data.Map.Strict (Map, mapKeys)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, extension, SafeCopy(version, kind), Migrate(..), safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack)
import SeeReason.Errors as Errors (Member, OneOf, set)

{-
-- | A map from a type fingerprint ('Fingerprint') to a wrapped value ('ByteString') of that type.
newtype Enc a = Enc a deriving (Generic, Monoid, Semigroup, Serialize, Eq, Ord)

enc :: Iso' (Enc s) s
enc = iso (\(Enc s) -> s) Enc
-}

newtype EncodedCache_0 =
  EncodedCache_0 {unEncodedCache_0 :: Map Fingerprint (Map ByteString ByteString)}
  deriving (Generic, Show, Eq, Ord, Data, Typeable)

instance SafeCopy EncodedCache_0 where version = 0; kind = base
deriving instance Serialize EncodedCache_0
instance Migrate EncodedCache where
  type MigrateFrom EncodedCache = EncodedCache_0
  migrate (EncodedCache_0 mp) = EncodedCache (fmap (\m -> (m, Nothing)) mp)

newtype EncodedCache =
  EncodedCache {unEncodedCache :: Map Fingerprint (Map ByteString ByteString, Maybe String {- human readable type info -})}
  deriving (Generic, Show, Eq, Ord, Data, Typeable)

instance SafeCopy EncodedCache where version = 1; kind = extension
instance Serialize EncodedCache where get = safeGet; put = safePut

instance Monoid EncodedCache where
  mempty = EncodedCache mempty

instance Semigroup EncodedCache where
  EncodedCache a <> EncodedCache b = EncodedCache (a <> b)

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

-- | This allows the types in the cache to be restricted, which
-- is helps keep track of what might or might not be in there.
class (SafeCopy k, Ord k, SafeCopy v) => EncodedValue k v

type EncodedValue' k v s = (EncodedValue k v, HasEncodedCache s)

mapLensE :: forall k v. (EncodedValue k v, HasCallStack) => Lens' EncodedCache (Map k v)
mapLensE =
  l1 . l2
  where
    l1 :: Lens' EncodedCache (Map ByteString ByteString)
    l1 = #unEncodedCache . at (typeRepFingerprint (typeRep (Proxy @(Map k v)))) . non mempty . _1
    l2 :: Iso' (Map ByteString ByteString) (Map k v)
    l2 = iso decodeMap encodeMap
    _ = callStack

mapLensE' :: forall k v s. (HasEncodedCache s, EncodedValue k v, HasCallStack) => Lens' s (Map k v)
mapLensE' = encodedCache . mapLensE @k @v

mapLens :: forall k v s. (HasEncodedCache s, EncodedValue k v, HasCallStack) => Lens' s (Map k v)
mapLens = mapLensE'

-- | Given a default, build a lens that points to the value of that
-- type in the 'EncodedCache'
--
-- @
-- > view (anyLens \'a\') $ (anyLens \'a\' %~ succ . succ) (mempty :: Dyn)
-- \'c\'
-- @
atLensE :: forall k v. (EncodedValue k v, HasCallStack) => k -> Lens' EncodedCache (Maybe v)
atLensE k =
  l1 . l2 . l3
  where
    l1 :: Lens' EncodedCache (Map ByteString ByteString)
    l1 = #unEncodedCache . at (typeRepFingerprint (typeRep (Proxy @(Map k v)))) . non mempty . _1
    l2 :: Lens' (Map ByteString ByteString) (Maybe ByteString)
    l2 = at (safeEncode k)
    l3 :: Iso' (Maybe ByteString) (Maybe v)
    l3 = iso decode' (fmap safeEncode)
    decode' :: Maybe ByteString -> Maybe v
    decode' (Just bs) = either (\_ -> Nothing {- This should not happen -}) Just (safeDecode bs)
    decode' Nothing = Nothing
    _ = callStack
{-# INLINE atLensE #-}

atLensE' :: forall k v s. (HasEncodedCache s, EncodedValue k v, HasCallStack) => k -> Lens' s (Maybe v)
atLensE' k = encodedCache . atLensE @k @v k

atLens :: forall k v s. (HasEncodedCache s, EncodedValue k v, HasCallStack) => k -> Lens' s (Maybe v)
atLens = atLensE'

anyLens :: forall s a. (EncodedValue' () a s, Eq a, HasCallStack) => a -> Lens' s a
anyLens d = atLens () . non d

maybeLens :: forall s a. (EncodedValue' () (Maybe a) s, Eq a, HasCallStack) => Lens' s (Maybe a)
maybeLens = anyLens (Nothing :: Maybe a)

-- | 'anyLens' for a value with a 'Default' instance.
defaultLensE :: forall k v. (Default v, Eq v, EncodedValue k v, HasCallStack) => k -> Lens' EncodedCache v
defaultLensE k = atLensE @k @v k . non def

-- | 'anyLens' for any value with a 'Bounded' instance.
boundedLensE ::
  forall k v. (Bounded v, Eq v, EncodedValue k v, HasCallStack)
  => k -> Lens' EncodedCache v
boundedLensE k = atLensE @k @v k . non (minBound :: v)

-- | 'anyLens' for any value with a 'Monoid' instance.
monoidLensE ::
  forall k v. (Monoid v, Eq v, EncodedValue k v, HasCallStack)
  => k -> Lens' EncodedCache v
monoidLensE k = atLensE @k @v k . non (mempty :: v)

ixLensE :: forall k v. (EncodedValue k v, HasCallStack) => k -> Traversal' EncodedCache v
ixLensE k = atLensE k . _Just

ixLensE' :: forall k v s. (HasEncodedCache s, EncodedValue k v, HasCallStack) => k -> Traversal' s v
ixLensE' k = encodedCache . ixLensE @k @v k

ixLens :: forall k v s. (HasEncodedCache s, EncodedValue k v, HasCallStack) => k -> Traversal' s v
ixLens = ixLensE'

mapPathE ::
  forall k v. (SafeCopy k, SafeCopy v, HasCallStack)
  => PathTo 'L EncodedCache (Map ByteString ByteString, Maybe String)
mapPathE =
  newtypePath <->
  atPath (typeRepFingerprint (typeRep (Proxy @(Map k v)))) <->
  nonPath (mempty, Just (show (typeRep (Proxy @(Map k v)))))

-- | It would be great to have a path that could go from ByteString to
-- the decoded type a, but at the moment we don't.  So this stops at
-- the 'ByteString'.
atPathE ::
  forall v k. (SafeCopy k, SafeCopy v, HasCallStack)
  => k
  -> PathTo 'L EncodedCache (Maybe ByteString)
atPathE k = mapPathE @k @v <-> fstPath <-> atPath (safeEncode k)

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
  => (forall o b. (Value b, IsGetterTag o) => PathTo o db b -> h b)
  -> h (Map k v)
queryEncodedMap queryDatumByGetter =
  queryDatumByGetter (encodedCachePath @db <-> mapPathE @k @v <-> fstPath) >>= \bs ->
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
                      (encodedCachePath @db <-> mapPathE @k @v <-> fstPath)
                      (fmap safeEncode (mapKeys safeEncode m)))

-- | Retrieve the encoded cache content for type a from the server.
queryEncodedAt ::
  forall db k v h e.
  (Monad h,
   MonadIO h,
   -- Value db,
   -- EventHandler h,
   HasEncodedCachePath db,
   Member PathError e,
   MonadError (OneOf e) h,
   SafeCopy k,
   SafeCopy v,
   HasCallStack)
  => (forall b o. (Value b, IsGetterTag o) => PathTo o db b -> h b)
  -> k
  -> h (Maybe v)
queryEncodedAt queryDatumByGetter k =
  queryDatumByGetter (encodedCachePath @db <-> atPathE @v k) >>= \case
    Nothing -> pure Nothing                             -- is a clean
    Just bs ->
      case safeDecode bs of
        Left s -> throwError (Errors.set (PathError s)) -- is a dirty
        Right m -> pure (Just m)                        -- is a clean

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

-- | Retrieve a map entry from the local 'EncodedCache'.
getEncoded ::
  forall v k s h. (MonadState s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> h (Maybe v)
getEncoded k = use (encodedCache . atLensE @_ @v k)

-- | Retrieve a map entry from the local 'EncodedCache'.
useEncoded ::
  forall v k a s h. (MonadState s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> Getter (Maybe v) a -> h a
useEncoded k lns = use (encodedCache . atLensE @_ @v k . lns)

-- | Retrieve a map entry from the local 'EncodedCache'.
askEncoded ::
  forall v k s h. (MonadReader s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> h (Maybe v)
askEncoded k = view (encodedCache . atLensE @_ @v k)

-- | Retrieve a map entry from the local 'EncodedCache'.
viewEncoded ::
  forall v k a s h. (MonadReader s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> Getter (Maybe v) a -> h a
viewEncoded k lns = view (encodedCache . atLensE @_ @v k . lns)

-- | Retrieve a map entry from the local 'EncodedCache'.
previewEncoded ::
  forall v k a s h. (MonadReader s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> Fold (Maybe v) a -> h (Maybe a)
previewEncoded k lns = preview (encodedCache . atLensE @_ @v k . lns)

-- | Retrieve a map entry from the local 'EncodedCache'.
preuseEncoded ::
  forall v k a s h. (MonadState s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> Fold (Maybe v) a -> h (Maybe a)
preuseEncoded k lns = preuse (encodedCache . atLensE @_ @v k . lns)

-- | Set a map entry in the local 'EncodedCache'.  The risk of this
-- function is making the local cache different from the remote.
putEncoded ::
  forall v k s h. (MonadState s h, EncodedValue k v, HasEncodedCache s, HasCallStack)
  => k -> Maybe v -> h ()
putEncoded k mv = encodedCache . atLensE @_ @v k .= mv

-- | Obtain a value from the server and add it to to local cache.
recvEncoded ::
  forall v k s h. (EncodedValue k v, HasEncodedCache s, MonadState s h, HasCallStack)
  => (k -> h (Maybe v)) -> k -> h (Maybe v)
recvEncoded query k = do
  mv <- query k
  putEncoded k mv
  pure mv

-- | Copy the EncodedCache from the server into MyView.  This should
-- actually copy a value associated with the effective or logged user.
-- WARNING: the only gets loaded once, if the value on the server is
-- changed by another client, the client this is running in will not
-- know.
recvEncodedCache ::
  forall db s h.
  (MonadState s h,
   HasEncodedCache s,
   -- HasEncodedCachePath db,
   HasCallStack) => h EncodedCache -> h ()
recvEncodedCache query =
  (.=) encodedCache =<< query
  where _ = callStack

-- | Update the local 'EncodedCache' value and also send it to the
-- server.  Skip the send when v is unchanged.
sendEncoded ::
  forall v k s h. (EncodedValue k v, HasEncodedCache s, MonadState s h, Eq v, HasCallStack)
  => (k -> Maybe v -> h ()) -- ^ Update the server value
  -> k
  -> Maybe v
  -> h ()
sendEncoded update k new = do
  old <- getEncoded @v k
  when (old /= new) $ do
    putEncoded k new
    update k new

-- | Apply a function to some part of v and update the value on the
-- server.  Skip the send when v is unchanged.
overEncoded ::
  forall v k a s h. (EncodedValue k v, HasEncodedCache s, MonadState s h, Eq v, HasCallStack)
  => (k -> Maybe v -> h ()) -- ^ Update the server value
  -> k
  -> Lens' (Maybe v) a
  -> (a -> a)
  -> h ()
overEncoded update k lns f = do
  old <- getEncoded @v k
  encodedCache . atLensE @_ @v k . lns %= f
  new <- getEncoded @v k
  when (old /= new) (update k new)
