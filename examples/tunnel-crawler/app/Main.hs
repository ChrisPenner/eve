module Main where

import Lib
import Extension

main :: IO ()
main = runCrawler $ do
         keepScore
