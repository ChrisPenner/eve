{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Internal.Extensions
  ( Exts
  , HasExts(..)
  , HasEvents
  , ext
  ) where

import Control.Lens
import Data.Map
import Unsafe.Coerce
import Data.Maybe
import Data.Typeable
import Data.Default

data Ext =
  forall ext. (Typeable ext, Show ext) =>
              Ext ext

instance Show Ext where
  show (Ext a) = show a

-- | A map of extension types to their current value.
type Exts = Map TypeRep Ext

class HasExts s  where
  exts :: Lens' s Exts

-- | A typeclass to ensure people don't dispatch events to states which shouldn't
--   accept them.
class (Typeable s, HasExts s) =>
      HasEvents s

ext
  :: forall a e.
    (Show a, Typeable a, Default a, HasExts e)
  => Lens' e a
ext = lens getter setter
  where
    getter s =
      fromMaybe def $ s ^. exts . at (typeRep (Proxy :: Proxy a)) . mapping coerce
    setter s new =
      set (exts . at (typeRep (Proxy :: Proxy a)) . mapping coerce) (Just new) s
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext
-- mExt
--   :: forall a e.
--     (Typeable a, Default a, HasExts e)
--       => Lens' e (Maybe a)
-- mExt = lens getter setter
--   where
--     getter s =
--       fromMaybe def $ s ^.exts . at (typeRep (Proxy :: Proxy a)) .
--       mapping coerce
--     setter s new =
--       set
--         (exts . at (typeRep (Proxy :: Proxy a)) . mapping coerce)
--         (Just new)
--         s
--     coerce = iso (\(Ext x) -> cast x) Ext
