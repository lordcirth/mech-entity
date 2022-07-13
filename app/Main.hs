module Main where

import           Brick.Main                (defaultMain)
import           BrickApp
import           Components ()
import           InitialState (initialState)
import           Logic (handleEvent)

import           Control.Monad.Trans.State
import           System.Random             (mkStdGen, randomRIO)
import           InitialState

main :: IO ()
main = putStrLn $ show w
