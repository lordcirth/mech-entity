module Components
where

import qualified Data.Map.Strict as Map

newtype ID = ID Int
  deriving (Show, Eq)

data Part = Part {}
  deriving (Show, Eq)
data Weapon = Weapon {}
  deriving (Show, Eq)

data World = World {
  part     :: Map.Map ID Part
  ,weapons :: Map.Map ID Weapon
}
  deriving (Show, Eq)

w = World {
  part = Map.empty
  ,weapons = Map.empty
}
