module InitialState where

import           Components
import           Constructors
import qualified Data.Map.Strict as Map

scrapMetal = ID 001

player = ID 0
maintDrone = ID 1001
maintDroneChassis = maintDrone


playerChassis = player


-- playerChassis = ID 90
-- playerArmor = ID 91

fist = ID 100
primitive20mmCannon = ID 101

metas :: Map.Map ID Meta
metas = Map.fromList [
  (scrapMetal, Meta {name = "Scrap Metal", description = "Basic Crafting Material"})
  ,(maintDrone, Meta {name = "Maintenance Drone", description = "" })
  ,(player, Meta {name = "Player", description = "Player"})
  ,(fist, Meta {name = "Fist", description = ""})
  ,(primitive20mmCannon, Meta {name = "Primitive 20mm Cannon", description = "Single-shot cannon"})
  ]

equips :: Map.Map ID Equipment
equips = Map.fromList [
  (playerChassis, Equipment {hp = 15, maxHp = 15, mass = 15, slots = 15})
  ,(maintDroneChassis, Equipment {hp = 10, maxHp = 10, mass = 10, slots = 10})
  -- Hits are never rolled directly to armor
  -- ,(playerArmor, Equipment {hp = 15, mass = 10, slots = 0})
  ,(fist, Equipment {hp = 0, maxHp = 0, mass = 0, slots = 0})
  ,(primitive20mmCannon, Equipment {hp = 5, maxHp = 5, mass = 5, slots = 3})
  ]

stacks :: Map.Map ID Stack
stacks = Map.fromList [
  (scrapMetal, Stack {num = 0})
  ]

weapons :: Map.Map ID Weapon
weapons = Map.fromList [
  (fist, Weapon {damage = newDamage 1 4 0}),
  (primitive20mmCannon, Weapon {damage = newDamage 1 6 2})
  ]

playerUnit :: Unit
playerUnit = Unit {
  components = [playerChassis, fist, primitive20mmCannon]
  ,armor = 15
  ,maxArmor = 15
  ,loot = []
}

maintDroneUnit = Unit {
  components = []
  ,armor = 5
  ,maxArmor = 5
  ,loot = [scrapMetal]
}


units :: Map.Map ID Unit
units = Map.fromList [(player, playerUnit), (maintDrone, maintDroneUnit)]

initialState = World {
  consumable = Map.empty
  ,equip = equips
  ,eventLog = ["Game Start"]
  ,unit = units
  ,status = PathSelect
  ,meta = metas
  ,stack = stacks -- Multiple items per instance
  ,weapon = weapons
  ,currentLoot = []
  ,paths = [PathNode {enemy = Just maintDrone, branches = []}]
}
