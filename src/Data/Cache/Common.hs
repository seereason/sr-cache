{-# LANGUAGE CPP #-}
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
  , AnyLens(anyLens)
  , MaybeLens(maybeLens)
  , HasMap(mapLens, atLens, atLensM)
  , defaultLens
  , boundedLens
  , monoidLens
  , ixLens
  ) where

import Control.Lens (at, _Just, Lens', non, ReifiedLens(Lens), ReifiedLens', Traversal')
import Data.ByteString (ByteString)
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (runPut, runGet, Serialize(get, put))
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.Fingerprint (Fingerprint(..))

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
instance SafeCopy Fingerprint

-- | Generic lens, allows access to a single @a@ inside a value @s2.
-- This and other classes in this module are used to break import
-- cycles by allowing the use of s without actually having its
-- declaration.
class AnyLens s a where
  anyLens :: HasCallStack => a -> Lens' s a

-- | Generic 'Maybe' lens
class MaybeLens s a where
  maybeLens :: Lens' s (Maybe a)

-- | Generic instance of 'AtLens'.
instance AnyLens s (Maybe a) => MaybeLens s a where
  maybeLens = anyLens (Nothing :: Maybe a)

-- | Generic 'Map' lens.
class HasMap k v s where
  mapLens :: HasCallStack => Lens' s (Map k v)
  atLens :: HasCallStack => k -> Lens' s (Maybe v)
  -- ^ Accees an element of a map
  atLensM :: (Monad m, HasCallStack) => m k -> m (ReifiedLens' s (Maybe v))

-- | Generic instance of 'HasMap'.
instance (AnyLens s (Map k v), Ord k, Typeable k, Typeable v) => HasMap k v s where
  mapLens = anyLens mempty
  atLens k = mapLens @k @v . at k
  atLensM k = do
    k' <- k
    pure $ Lens $ atLens k'

-- | 'anyLens' for a value with a 'Default' instance.
defaultLens :: forall a s. (AnyLens s a, Default a, HasCallStack) => Lens' s a
defaultLens = Data.Cache.Common.anyLens @s @a def

boundedLens ::
  forall k v s. (AnyLens s (Map k v), Ord k, Typeable k, Typeable v, Bounded v, Eq v, HasCallStack)
  => k -> Lens' s v
boundedLens k = Data.Cache.Common.atLens @k @v k . non (minBound :: v)

monoidLens ::
  forall k v s. (AnyLens s (Map k v), Ord k, Typeable k, Typeable v, Monoid v, Eq v, HasCallStack)
  => k -> Lens' s v
monoidLens k = Data.Cache.Common.atLens @k @v k . non (mempty :: v)

ixLens :: forall k v s. (AnyLens s (Map k v), Ord k, Typeable k, Typeable v, HasCallStack) => k -> Traversal' s v
ixLens k = atLens k . _Just
