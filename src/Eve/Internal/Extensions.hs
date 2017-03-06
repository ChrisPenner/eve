{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eve.Internal.Extensions
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

-- | A wrapper to allow storing differing extensions in the same place.
data Ext =
  forall ext. (Typeable ext, Show ext) =>
              Ext ext

instance Show Ext where
  show (Ext a) = show a

-- | A map of extension types to their current value.
type Exts = Map TypeRep Ext

-- | Represents a state which can be extended.
-- 'exts' is a 'Lens'' which points to the state's 'Exts'
class HasExts s  where
  exts :: Lens' s Exts

-- | A typeclass to ensure people don't dispatch events to states which shouldn't
--   accept them.
--
-- To allow dispatching events in an action over your state simply define the
-- empty instance:
--
-- > instance HasEvents MyState where
-- -- Don't need anything here.
class (Typeable s, HasExts s) =>
      HasEvents s

-- | A polymorphic lens which accesses extensions in the extension state.
-- It returns the default value ('def') if a state has not yet been set.
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
