module Util where

import           Components
import           Control.Monad.Trans.State (State, execState, get, put)
import           Data.Char                 (intToDigit)
import           Data.List                 (intersect)
import           Data.List.Index           (deleteAt, imap)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, listToMaybe)
import           InitialState              (maintDrone, player)

-- Make a copy of all object properties and return new ID
clone :: ID -> State World ID
clone templateID = do
  w <- get
  let c = Map.lookup templateID w.consumable :: Maybe Consumable
  put w{consumable = maybe w.consumable (\x -> Map.insert newID x w.consumable) c}
--  f equip
--  f unit
--  f meta
--  f stack
--  f weapon
  return newID

  where
    newID = templateID + 1000 -- TODO
--    f :: (World -> Map.Map ID a) -> State World ()
--    f m = do
--      w <- get
--      let entry = getEntry w
--      put w{m = m w}
--
--      return ()
--      where
--        getEntry w = Map.lookup templateID (m w) -- :: Maybe a


getEnemy :: World -> ID
getEnemy w = maintDrone -- TODO

getPlayer :: World -> ID
getPlayer w = player

isDead :: World -> ID -> Bool
isDead w u =
  (fromJust $ Map.lookup u w.equip).hp <= 0

getWeapons :: World -> ID -> [ID]
getWeapons w u = intersect c ws
  where
    c = (fromJust $ Map.lookup u (w.unit)).components
    ws = Map.keys (w.weapon)

applyDamage :: Int -> ID -> State World ()
applyDamage dmg target = do
  w <- get
  let e = fromJust $ Map.lookup target w.equip :: Equipment
  let newEquip = e{hp = e.hp - dmg} :: Equipment

  put $ w{equip = (Map.insert target newEquip w.equip)}

combatActions :: World -> ID -> ID -> [Action]
combatActions w actor target = imap (attackAction w actor target) $ getWeapons w actor

lootActions :: World -> ID -> ID -> [Action]
lootActions w actor target = doneLoot w : imap (lootAction w actor target) w.currentLoot

doneLoot :: World -> Action
doneLoot w = Action {
  effect = do
    w <- get
    -- discard remaining loot & leave screen
    put w{currentLoot = [] }
    setStatus Tinker

  ,item = Nothing
  ,key = '.'
  ,name = "Done looting"
}


lootAction :: World -> ID -> ID -> Int -> ID -> Action
lootAction w actor target key item = Action {
  effect  = do
    addItem item 1
    w <- get
    let newLoot = deleteAt key w.currentLoot
    put w{currentLoot = newLoot}
  ,item   = Just item
  ,key    = intToDigit key
  ,name   = getName w item
}

tinkerActions :: World -> ID -> ID -> [Action]
tinkerActions w actor target = doneTinker w : [] -- imap (tinkerAction w actor target) w.currentLoot

doneTinker :: World -> Action
doneTinker w = Action {
  effect = do
    setStatus PathSelect

  ,item = Nothing
  ,key = '.'
  ,name = "Done tinkering"
}

pathActions :: World -> ID -> ID -> [Action]
pathActions w _ _ = imap (pathSelectAction w) w.location.branches

pathSelectAction :: World -> Int -> PathNode -> Action
pathSelectAction w path _ = Action {
  effect  = do
    -- Set new location
    w <- get
    put w{location = (w.location.branches !! path)}

    -- Replace enemy ID with clone
    w <- get
    e <- clone (fromJust $ w.location.enemy)
    put w{location = w.location{enemy = Just e}}

    setStatus (Combat PlayerTurn) -- TODO check if enemy exists

  ,item = Nothing
  ,key  = intToDigit path
  ,name = [intToDigit path]
}

getName :: World -> ID -> String
getName w item = maybe "UNDEFINED" (\m -> m.name) (Map.lookup item w.meta)

setStatus :: GameStatus -> State World ()
setStatus newStatus = do
    w <- get
    put $ w{status = newStatus}

checkCombatEnd :: State World Bool
checkCombatEnd = do
  w <- get
  let oldStatus = w.status
  let newStatus = checkStatus w
  if newStatus /= oldStatus then do
    let newLoot = (fromJust $ Map.lookup (getEnemy w) w.unit).loot
    put $ w{currentLoot = newLoot}
    setStatus newStatus
    return True
  else do
    return False

checkStatus :: World -> GameStatus
checkStatus w
    | isDead w (getPlayer w) = GameOver
    | isDead w (getEnemy w) = LootScreen
    | otherwise = w.status

matchKey :: [Action] -> Char -> Maybe Action
matchKey acts c = listToMaybe $ filter (\a -> a.key == c) acts


doAction :: Maybe Action -> State World ()
doAction Nothing       = return ()
doAction (Just action) = action.effect

event :: String -> State World ()
event msg = do
  w <- get
  put w{eventLog = msg:w.eventLog}

addItem :: ID -> Int -> State World ()
addItem item n = do
  w <- get
  let s = Map.lookup item w.stack
  let c = n + (maybe 0 (\x -> x.num) s)

  let newStack = Stack{num = c}
  put $ w{stack = Map.insert item newStack w.stack}
  event $ "Gained item, new total: " ++ show c

attackAction :: World -> ID -> ID -> Int -> ID -> Action
attackAction w actor target key item = Action {
  effect  = attackEffect actor target
  ,item   = Just item
  ,key    = intToDigit key
  ,name   = getName w item
}
  where
    -- Target is assumed to be a Unit
    attackEffect :: ID -> ID -> State World ()
    attackEffect actor target = do
      event $ (show $ getName w actor) ++ " attacks " ++
        (show $ getName w target)
      applyDamage 1 target

