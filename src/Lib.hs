module Lib
    ( someFunc
    ) where

import System.TimeIt (timeIt)

import Parallel (twoMeans, twoMeansPar)

someFunc = do
  timeIt . putStrLn . show $ twoMeansPar 1234567
  timeIt . putStrLn . show $ twoMeans 1234567
  timeIt . putStrLn . show $ twoMeansPar 1234568
  timeIt . putStrLn . show $ twoMeans 1234568
  -- putStrLn $ show $ length $ parSort [1..134]
