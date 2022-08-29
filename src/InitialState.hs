module InitialState where

import           Components
import           Constructors
import qualified Data.Map.Strict as Map

scrapMetal = ID 001

player = ID 0
maintDrone = ID 1001
maintDroneChassis = ID 2001

playerChassis = ID 90
playerArmor = ID 91

primitive20mmCannon = ID 101

metas :: Map.Map ID Meta
metas = Map.fromList [
  (scrapMetal, Meta {name = "Scrap Metal", description = "Basic Crafting Material"})
  ,(primitive20mmCannon, Meta {name = "Primitive 20mm Cannon", description = "Single-shot cannon"})
  ]

equips :: Map.Map ID Equipment
equips = Map.fromList [
  (playerChassis, Equipment {hp = 15, mass = 15, slots = 15})
  ,(maintDroneChassis, Equipment {hp = 10, mass = 10, slots = 10})
  -- Hits are never rolled directly to armor
  -- ,(playerArmor, Equipment {hp = 15, mass = 10, slots = 0})
  ,(primitive20mmCannon, Equipment {hp = 5, mass = 5, slots = 3})
  ]

stacks :: Map.Map ID Stack
stacks = Map.fromList [
  (scrapMetal, Stack {num = 0})
  ]

weapons :: Map.Map ID Weapon
weapons = Map.fromList [
  (primitive20mmCannon, Weapon {damage = newDamage 1 6 2})
  ]

playerUnit :: Unit
playerUnit = Unit {
  components = [playerChassis, primitive20mmCannon]
  ,armor = 15
  ,maxArmor = 15
}

maintDroneUnit = Unit {
  components = []
  ,armor = 5
  ,maxArmor = 5
}


units :: Map.Map ID Unit
units = Map.fromList [(player, playerUnit), (maintDrone, maintDroneUnit)]

initialState = World {
  consumable = Map.empty
  ,equip = equips
  ,unit = units
  ,status = Combat PlayerTurn
  ,meta = metas
  ,stack = stacks -- Multiple items per instance
  ,weapon = weapons
}
