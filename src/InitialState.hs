module InitialState where

import           Components
import qualified Data.Map.Strict as Map

scrapMetal = ID 000

primitive20mmCannon = ID 100

metas :: Map.Map ID Meta
metas = Map.fromList [
  (scrapMetal, Meta {name = "Scrap Metal", description = "Basic Crafting Material"})
  ,(primitive20mmCannon, Meta {name = "Primitive 20mm Cannon", description = "Single-shot cannon"})
  ]

equips :: Map.Map ID Equipment
equips = Map.fromList [
  (primitive20mmCannon, Equipment {hp = 5, mass = 5, slots = 3})
  ]

stacks :: Map.Map ID Stack
stacks = Map.fromList [
  (scrapMetal, Stack {num = 0})
  ]


w = World {
  meta = metas
  ,consumable = Map.empty
  ,equip = equips
  ,stack = stacks -- Multiple items per instance
  ,weapons = Map.empty
}
