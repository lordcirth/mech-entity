module InitialState where

import           Components
import qualified Data.Map.Strict as Map

w = World {
  meta = Map.empty
  ,part = Map.empty
  ,weapons = Map.empty
}
