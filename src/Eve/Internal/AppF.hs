{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
-- {-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
module Eve.Internal.AppF
  ( Action(..)
  , ActionF(..)
  , App
  , AppState(..)
  , exit
  , isExiting
  , Exiting(..)
  , asyncQueue
  , liftAction
  , execApp
  , runAction
  ) where

import Eve.Internal.Extensions
import Data.Default

import Control.Monad.State
import Control.Monad.Free
import Control.Lens

import Pipes.Concurrent

type App a = Action AppState a
data AppState = AppState
  { _baseExts :: Exts
  , _asyncQueue :: Output (App ())
  }

data ActionF zoomed next =
    LiftAction (Action AppState next)
    | LiftIO (IO next)
    | StateAction (StateT zoomed IO next)
    deriving Functor

newtype Action zoomed a = Action
  { getAction :: Free (ActionF zoomed) a
  } deriving (Functor, Applicative, Monad)

makeLenses ''AppState

instance HasExts AppState where
  exts = baseExts

instance HasEvents AppState where

liftActionF :: ActionF zoomed next -> Action zoomed next
liftActionF = Action . liftF

instance MonadState zoomed (Action zoomed) where
  state = liftActionF . StateAction . state

liftAction :: Action AppState a -> Action zoomed a
liftAction = liftActionF . LiftAction

execApp :: Action AppState a -> StateT AppState IO a
execApp (Action actionF) = foldFree toState actionF
  where
    toState (LiftAction act) = execApp act
    toState (LiftIO io) = liftIO io
    toState (StateAction st) = st

type instance Zoomed (Action s) = Zoomed (StateT s IO)
instance Zoom (Action s) (Action t) s t where
  zoom l (Action actionF) = Action $ hoistFree (zoomActionF l) actionF
    where
      zoomActionF _ (LiftAction act) = LiftAction act
      zoomActionF _ (LiftIO io) = LiftIO io
      zoomActionF lns (StateAction act) = StateAction $ zoom lns act

-- runAction :: Lens' base zoomed -> Action zoomed a -> Action base a
-- runAction l (Action actionF) = Action $ hoistFree (zoomActionF l) actionF
--   where
--     zoomActionF :: Lens' base zoomed -> ActionF zoomed a -> ActionF base a
--     zoomActionF _ (LiftAction la) = LiftAction la
--     zoomActionF _ (LiftIO io) = LiftIO io
--     zoomActionF lns (StateAction act) = StateAction $ zoom lns act

newtype Exiting =
  Exiting Bool
  deriving (Show, Eq)

instance Default Exiting where
  def = Exiting False

exit :: App ()
exit = ext .= Exiting True

isExiting :: App Bool
isExiting = do
  Exiting b <- use ext
  return b
