module Util where

import           Components
import           Data.List       (intersect)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

getWeapons :: World -> ID -> [ID]
getWeapons w u = intersect c ws
  where
    c = (fromJust $ Map.lookup u (w.unit)).components
    ws = Map.keys (w.weapon)
