{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Extension where

import Eve
import Control.Lens
import Lib
import Data.Default

data Score = Score
  { _score' :: Int
  }
makeLenses ''Score

instance Default Score where
  def = Score 0

score :: Lens' AppState Int
score = makeStateLens score'

addPoint :: App ()
addPoint = score += 1

displayScore :: App [String]
displayScore = do
  s <- use score
  return ["Score: " ++ show s]

keepScore :: App ()
keepScore = do
  addListener_ (const addPoint :: TreasureCollected -> App ())
  provideStatusInfo displayScore
