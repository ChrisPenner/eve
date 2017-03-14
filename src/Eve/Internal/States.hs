{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eve.Internal.States
  ( States
  , HasStates(..)
  , HasEvents
  , stateLens
  ) where

import Control.Lens
import Data.Map
import Unsafe.Coerce
import Data.Maybe
import Data.Typeable
import Data.Default

-- | A wrapper to allow storing types of states in the same place.
data StateWrapper =
  forall s. (Typeable s) =>
              StateWrapper s

-- | A map of state types to their current value.
type States = Map TypeRep StateWrapper

-- | Represents a state which can itself store more states.
-- 'states' is a lens which points to a given state's 'States' map.
class HasStates s  where
  states :: Lens' s States

-- | A typeclass to ensure people don't dispatch events to states which shouldn't
--   accept them.
--
-- To allow dispatching events in an action over your state simply define the
-- empty instance:
--
-- > instance HasEvents MyState where
-- -- Don't need anything here.
class (Typeable s, HasStates s) =>
      HasEvents s

-- | A polymorphic lens which accesses stored states.
-- It returns the default value ('def') if a state has not yet been set.
stateLens
  :: forall a e.
    (Typeable a, Default a, HasStates e)
  => Lens' e a
stateLens = lens getter setter
  where
    getter s =
      fromMaybe def $ s ^. states . at (typeRep (Proxy :: Proxy a)) . mapping coerce
    setter s new =
      set (states . at (typeRep (Proxy :: Proxy a)) . mapping coerce) (Just new) s
    coerce = iso (\(StateWrapper x) -> unsafeCoerce x) StateWrapper
