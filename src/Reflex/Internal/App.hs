{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
module Reflex.Internal.App
  ( App
  , Action(..)
  , coerceAction
  , AppState(..)
  , exit
  , isExiting
  , Exiting(..)
  , asyncQueue
  -- , runActionOver
  , liftAction
  , execApp
  , runAction
  ) where

import Reflex.Internal.Extensions

import Control.Monad.State
import Control.Lens

import Data.Default
import Pipes.Concurrent

data AppState = AppState
  { _baseExts :: Exts
  , _asyncQueue :: Output (App ())
  }

data AppLens s t =
  AppLens (Lens' s t)

newtype Action base zoomed a = Action
  { getAction :: AppLens base zoomed -> StateT base IO a
  }

type App a = StateT AppState IO a

makeLenses ''AppState

instance HasExts AppState where
  exts = baseExts

instance HasEvents AppState where

instance MonadState zoomed (Action base zoomed) where
  get = Action $ \(AppLens l) -> zoom l get
  put n = Action $ \(AppLens l) -> zoom l (put n)

instance Functor (Action base zoomed) where
  fmap f (Action m) = Action $ fmap (fmap f) m

instance Applicative (Action base zoomed) where
  pure a = Action (\_ -> pure a)
  Action a <*> Action b = Action (\l -> a l <*> b l)

instance Monad (Action base zoomed) where
  Action a >>= f = Action (\l -> a l >>= (\(Action b) -> b l) . f)

coerceAction :: Lens' middle top -> Action middle top a -> Action bottom middle a
coerceAction l act = Action $
  \(AppLens l2) -> zoom l2 (runAction l act)
-- coerceAction l (Action act) = Action $
--   \(AppLens l2) -> zoom l2 (act (AppLens l))

liftAction :: App a -> Action AppState zoomed a
liftAction act = Action (const act)

runAction :: Lens' s zoomed -> Action s zoomed a -> StateT s IO a
runAction l (Action act) = act (AppLens l)

execApp :: AppState -> App a -> IO AppState
execApp = flip execStateT

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
