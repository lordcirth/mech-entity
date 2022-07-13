module InitialState where

import           Components
import           Constructors
import qualified Data.Map.Strict as Map

scrapMetal = ID 000

chassis = ID 90
armor = ID 91

primitive20mmCannon = ID 101

metas :: Map.Map ID Meta
metas = Map.fromList [
  (scrapMetal, Meta {name = "Scrap Metal", description = "Basic Crafting Material"})
  ,(primitive20mmCannon, Meta {name = "Primitive 20mm Cannon", description = "Single-shot cannon"})
  ]

equips :: Map.Map ID Equipment
equips = Map.fromList [
  (chassis, Equipment {hp = 15, mass = 15, slots = 15})
  -- Hits are never rolled directly to armor
  ,(armor, Equipment {hp = 15, mass = 10, slots = 0})
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

w = World {
  meta = metas
  ,consumable = Map.empty
  ,equip = equips
  ,stack = stacks -- Multiple items per instance
  ,weapon = weapons
}
