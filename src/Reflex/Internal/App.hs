module Reflex.Internal.App
  ( App(..)
  , Action
  , AppState(..)
  , exit
  , isExiting
  , Exiting(..)
  , asyncQueue
  -- , runActionOver
  , liftAction
  ) where

import Reflex.Internal.Extensions

import Control.Monad.State
import Control.Lens

import Data.Default
import Pipes.Concurrent

-- type Action s a = forall m. (Monad m, MonadState s m) => m a

liftAction :: MonadTrans m => App a -> m App a
liftAction = lift

-- type App a = StateT AppState IO a

-- newtype App m a = App
--   { runApp :: StateT AppState m a
--   } deriving (Functor, Applicative, Monad, MonadIO, MonadState AppState)


data AppState = AppState
  { _baseExts :: Exts
  , _asyncQueue :: Output (App ())
  }

newtype App m a = App
  { runApp :: StateT AppState m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState AppState)


-- newtype AppT m a = AppT { runAppT :: AppState -> m (a, AppState) }

-- instance (Functor m) => Functor (AppT m) where
--     fmap f m = AppT $ \ s ->
--         (\ ~(a, s') -> (f a, s')) <$> runAppT m s

-- instance (Functor m, Monad m) => Applicative (AppT m) where
--     pure = return
--     (<*>) = ap

-- instance (Monad m) => Monad (AppT m) where
--   return a = AppT $ \ s -> return (a, s)
--   m >>= k  = AppT $ \ s -> do
--       ~(a, s') <- runAppT m s
--       runAppT (k a) s'

-- instance MonadTrans AppT where
--   lift m = AppT $ \ s -> do
--     a <- m
--     return (a, s)

-- instance (MonadIO m) => MonadIO (AppT m) where
--     liftIO = lift . liftIO

instance HasExts AppState where
  exts = baseExts

instance HasEvents AppState where

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

-- runActionOver :: (MonadState s m, MonadState e n) => Lens' s e -> m r -> n r
-- runActionOver = zoom
