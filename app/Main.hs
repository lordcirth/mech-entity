module Main where

import           Brick.Main                (defaultMain)
import           BrickApp
import           Components                ()
import           InitialState              (initialState)
--import           Logic (handleEvent)

import           Control.Monad.Trans.State
import           InitialState
import           System.Random             (mkStdGen, randomRIO)

main :: IO ()
main = do
  let r = mkStdGen 12345
  let readyState = initialState{rng = r}

  -- Run main loop
  finalState <- defaultMain brickApp readyState
  print finalState
