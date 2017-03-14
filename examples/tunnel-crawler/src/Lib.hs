{-# LANGUAGE RankNTypes #-}
module Lib where

import Eve
import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Default

data KeyPress = KeyPress Char

data GameState = GameState Int

instance Default GameState where
  def = GameState 0


updatePos :: Char -> Action GameState ()
updatePos 'a' = modify dec
  where
    dec (GameState pos) = GameState $ pos - 1
updatePos 'd' = modify inc
  where
    inc (GameState pos) = GameState $ pos + 1
updatePos _ = return ()

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  runAction stateLens $ updatePos c
  GameState pos <- use stateLens
  liftApp . liftIO $ print pos

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
  addListener_ handleKeypress

gameLoop :: IO ()
gameLoop = eve_ setup


