module Main where

import           Control.Monad    (forM_)
import           Data.Traversable
import           Proc


main :: IO ()
main = do
  pids <- getCurrentPIDs
  procs <- traverse createProcess pids
  forM_ procs print
