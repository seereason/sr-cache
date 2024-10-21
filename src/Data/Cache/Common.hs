{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Bool (bool)
import Data.ByteString (ByteString, length)
import Data.Data (Data)
import Data.List (intersperse)
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (runPut, runGet, Serialize)
import Data.String (IsString(fromString))
import Debug.Trace (trace)
import GHC.Generics
import GHC.Fingerprint (Fingerprint(..))
import GHC.Stack (callStack, getCallStack, HasCallStack, SrcLoc(..))
import Prelude hiding (length)

traceIf :: (a -> Bool) -> (a -> String) -> a -> a
traceIf p s a = bool a (trace (s a) a) (p a)

-- | Serialize a value using a type's SafeCopy instance.
safeEncode :: (SafeCopy a, HasCallStack) => a -> ByteString
safeEncode a =
  traceIf
    (\bs -> length bs > 1000)
    (\bs -> "safeEncode " <> show (length bs) <> " bytes (" <> compactStack (getCallStack callStack) <> ")")
    (runPut (safePut a))

safeDecode :: (SafeCopy a, HasCallStack) => ByteString -> Either String a
safeDecode bs =
  traceIf
    (\_ -> length bs > 1000)
    (\_ -> "safeDecode " <> show (length bs) <> " bytes (" <> compactStack (getCallStack callStack) <> ")")
    (runGet safeGet bs)

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

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
compactStack :: forall s. (IsString s, Monoid s, HasCallStack) => [(String, SrcLoc)] -> s
compactStack [] = "(no CallStack)"
compactStack [(callee, loc)] = fromString callee <> " ← " <> srcloccol loc
compactStack [(_, loc), (caller, _)] = srcloccol loc <> "." <> fromString caller
compactStack ((_, loc) : more@((caller, _) : _)) =
  mconcat (intersperse (" ← " :: s)
            (-- fromString callee :
             srcfunloc loc (fromString caller) :
             stacktail (fmap snd more)))
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [loc'] = [srcloccol loc']
    stacktail (loc' : more') = srcloc loc' : stacktail more'
    _ = callStack

-- | With start column
srcloccol :: (IsString s, Semigroup s) => SrcLoc -> s
srcloccol loc = srcloc loc <> ":" <> fromString (show (srcLocStartCol loc))

-- | Compactly format a source location with a function name
srcfunloc :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloc loc f = fromString (srcLocModule loc) <> "." <> f <> ":" <> fromString (show (srcLocStartLine loc))

-- | Compactly format a source location
srcloc :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc loc = fromString (srcLocModule loc) <> ":" <> fromString (show (srcLocStartLine loc))
