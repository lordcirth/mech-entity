module Components
where

import qualified Data.Map.Strict as Map

type Name = ()

newtype ID = ID Int
  deriving (Show, Eq, Ord)

data Damage = Damage {
  dieNum   :: Int
  ,dieSize :: Int
  ,flatDmg :: Int
}
  deriving (Show, Eq)

data Meta = Meta {
  description :: String
  ,name       :: String
}
  deriving (Show, Eq)

data Unit = Unit {
  components :: [ID]
  ,armor     :: Int
  ,maxArmor  :: Int
}
  deriving (Show, Eq)

data Consumable = Consumable
  deriving (Show, Eq)

data Equipment = Equipment {
  hp     :: Int
  ,mass  :: Int -- kg
  ,slots :: Int
}
  deriving (Show, Eq)

data Stack = Stack {
  num :: Int
}
  deriving (Show, Eq)

data Weapon = Weapon {
  --ammoType :: ID
  damage :: Damage
}
  deriving (Show, Eq)

data GameStatus = Combat CombatSubStatus | LootScreen | Tinker | PathSelect | GameOver
  deriving (Show, Eq)

data CombatSubStatus = ReloadPrompt | PlayerTurn | EnemyTurn
  deriving (Show, Eq)

data World = World {
  status      :: GameStatus
  ,consumable :: Map.Map ID Consumable
  ,equip      :: Map.Map ID Equipment
  ,unit       :: Map.Map ID Unit
  ,meta       :: Map.Map ID Meta
  ,stack      :: Map.Map ID Stack
  ,weapon     :: Map.Map ID Weapon
}
  deriving (Show, Eq)
