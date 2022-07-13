module Constructors
where

import           Components

newDamage :: Int -> Int -> Int -> Damage
newDamage num size flat = Damage {
  dieNum = num
  ,dieSize = size
  ,flatDmg = flat
}
