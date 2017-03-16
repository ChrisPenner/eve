{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Eve
import System.IO
import System.Random
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Default
import Data.List

data Timer = Timer
data KeyPress = KeyPress Char
data TreasureCollected = TreasureCollected
data GetStatusInfo = GetStatusInfo

data GameState = GameState
  { _pos' :: Int
  , _treasures' :: [Int]
  }
makeLenses ''GameState

instance Default GameState where
  def = GameState 0 []

pos :: Lens' AppState Int
pos = makeStateLens pos'

treasures :: Lens' AppState [Int]
treasures = makeStateLens treasures'

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  case c of
    'a' -> pos -= 1
    'd' -> pos += 1
    _ -> return ()
  collectTreasure

keypressProvider :: EventDispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar
  dispatcher (KeyPress c)

-- A simple tunnel drawing
tunnel :: String
tunnel = replicate 20 '.'

-- Replace the char at a position with a new char
replaceAt :: Char -> Int -> String -> String
replaceAt _ _ [] = []
replaceAt c 0 (_:xs) = c:xs
replaceAt c n (x:xs) = x:replaceAt c (n-1) xs

render :: App ()
render = do
  liftIO $ putChar '\r'
  n <- use pos
  t <- use treasures
  statuses <- getStatusInfo
  let renderedTunnel = replaceAt '$' n . addTreasures t $ tunnel
      renderedStatuses = intercalate " | " statuses
  liftIO . putStr $ renderedTunnel ++ renderedStatuses
    where
      addTreasures t tunnel = foldr (replaceAt '%') tunnel t

timer :: EventDispatcher -> IO ()
timer dispatch = forever $ do
  threadDelay 3000000
  dispatch Timer

collectTreasure :: App ()
collectTreasure = do
  p <- use pos
  t <- use treasures
  when (p `elem` t) $ do
    treasures %= delete p
    dispatchEvent_ TreasureCollected

spawnTreasure :: App ()
spawnTreasure = do
  r <- liftIO $ randomRIO (1, 20)
  treasures %= (r:)

getStatusInfo :: App [String]
getStatusInfo = dispatchEvent GetStatusInfo

provideStatusInfo :: App [String] -> App ()
provideStatusInfo app = addListener_ (const app :: GetStatusInfo -> App [String])

setup :: App ()
setup = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
  asyncEventProvider keypressProvider
  addListener_ handleKeypress
  addListener_ (const spawnTreasure :: Timer -> App ())
  afterEvent_ render
  asyncEventProvider timer
  provideStatusInfo $ do
    p <- use pos
    return ["Position: " ++ show p]

runCrawler :: App () -> IO ()
runCrawler extensions = eve_ (setup >> extensions)
