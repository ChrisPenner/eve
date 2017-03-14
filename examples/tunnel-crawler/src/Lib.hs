{-# LANGUAGE RankNTypes #-}
module Lib where

import Eve
import System.IO
import Control.Monad
import Control.Monad.Trans

data KeyPress = KeyPress Char

keypressProvider :: Dispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar
  dispatcher (KeyPress c)

echo :: KeyPress -> App ()
echo (KeyPress c) = liftIO (print $ "You pressed: " ++ [c])

setup :: App ()
setup = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
  asyncEventProvider keypressProvider
  addListener_ echo

gameLoop :: IO ()
gameLoop = eve_ setup
