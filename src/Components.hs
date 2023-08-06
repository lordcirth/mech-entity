{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Components
where

import           Control.Monad.Trans.State (State)
import qualified Data.Map.Strict           as Map
import           System.Random             (StdGen)

type Name = ()

newtype ID = ID Int
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

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
  ,loot      :: [ID]
}
  deriving (Show, Eq)

data Consumable = Consumable
  deriving (Show, Eq)

data Equipment = Equipment {
  hp     :: Int
  ,maxHp :: Int
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

data Action = Action {
  effect :: State World ()
  ,item  :: Maybe ID
  ,key   :: Char
  ,name  :: String
}

data PathNode = PathNode {
  branches :: [PathNode]
  ,enemy   :: Maybe ID
}
instance Show (PathNode) where
  show p = show p.enemy

data World = World {
  status       :: GameStatus
  ,consumable  :: Map.Map ID Consumable
  ,equip       :: Map.Map ID Equipment
  ,eventLog    :: [String]
  ,unit        :: Map.Map ID Unit
  ,meta        :: Map.Map ID Meta
  ,stack       :: Map.Map ID Stack
  ,weapon      :: Map.Map ID Weapon
  ,currentLoot :: [ID]
  ,location    :: PathNode
  ,rng         :: StdGen
  ,enemy       :: Maybe ID
}
  deriving (Show)
