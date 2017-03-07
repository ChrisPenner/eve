{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
module Crawler
  ( gameLoop
  ) where

import Eve
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Default
import System.IO
import System.Random
import Control.Concurrent
import Data.Monoid
import Data.List

newtype KeyPress = KeyPress Char
data Timer = Timer
data Render = Render

data GameState = GameState
  { _pos' :: Int
  , _treasures' :: [Int]
  , _score' :: Int
  } deriving Show
makeLenses ''GameState

pos :: HasStates s => Lens' s Int
pos = stateLens.pos'

treasures :: HasStates s => Lens' s [Int]
treasures = stateLens.treasures'

score :: HasStates s => Lens' s Int
score = stateLens.score'

instance Default GameState where
  def = GameState 0 [] 0

keypressProvider :: Dispatcher -> IO ()
keypressProvider dispatcher = forever $ getChar >>= dispatcher . KeyPress

spawnTreasure :: App ()
spawnTreasure = do
  r <- liftIO $ randomRIO (1, 20)
  treasures %= (r:)
  dispatchEvent_ Render

timer :: Dispatcher -> IO ()
timer dispatch = forever $ do
  threadDelay 5000000
  dispatch Timer

replaceAt :: Int -> Char -> String -> String
replaceAt _ _ [] = []
replaceAt 0 c (x:xs) = c:xs
replaceAt n c (x:xs) = x:replaceAt (n-1) c xs

base :: String
base = replicate 20 '.'

render :: App ()
render = do
  n <- use pos
  t <- use treasures
  sc <- use score
  liftIO $ putChar '\r'
  liftIO . putStr $ replaceAt n '$' . appEndo (foldMap (Endo . flip replaceAt '%') t) $ base ++ "  Score: " ++ show sc

checkScore :: App ()
checkScore = do
  p <- use pos
  t <- use treasures
  when (p `elem` t) (treasures %= delete p >> score += 1)

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  case c of
    'l' -> pos %= (min 20 . (+1))
    'h' -> pos %= (max 0 . subtract 1)
    _ -> return ()
  checkScore
  dispatchEvent_ Render

gameLoop :: IO ()
gameLoop = eve $ do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
  render
  asyncEventProvider keypressProvider
  asyncEventProvider timer
  addListener_ handleKeypress
  addListener_ (const spawnTreasure :: Timer -> App ())
  addListener_ (const render :: Render -> App ())
