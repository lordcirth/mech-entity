module Util where

import           Components
import           Control.Monad.Trans.State (State, execState, get, put)
import           Data.List                 (intersect)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, listToMaybe)
import           InitialState              (maintDrone, player)

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

getName :: World -> ID -> String
getName w item = maybe "UNDEFINED" (\m -> m.name) (Map.lookup item w.meta)

updateStatus :: State World GameStatus
updateStatus = do
  w <- get
  put $ w{status = checkStatus w}
  return $ checkStatus w

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
