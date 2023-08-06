module Logic where

import qualified Brick                     as B
import           Components
import           Control.Monad.Trans.State (State, execState, get, put)
import           Data.Char                 (intToDigit)
import           Data.List.Index           (deleteAt, imap)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust)
import qualified Graphics.Vty              as V
import           InitialState              (player)
import           Util


handleEvent :: World -> B.BrickEvent Name () -> B.EventM Name (B.Next World)

-- Halt on ESC
handleEvent w (B.VtyEvent (V.EvKey (V.KEsc) _)) = B.halt w

handleEvent w (B.VtyEvent (V.EvKey (V.KChar keyPress) _))
  | Combat a <- w.status = process combatTurn
  | otherwise = process playerTurn
    where
      process :: (Char -> State World ()) -> B.EventM Name (B.Next World)
      process stage = B.continue $ execState (stage keyPress) w

-- no-op for all other inputs
handleEvent w _                                         = B.continue w

--
--generateActions :: World -> ID -> [Action]
--generateActions w actor =
--  mainActions w actor


combatTurn :: Char -> State World ()
combatTurn keyPress = do
  playerTurn keyPress
  endOfPlayerTurn <- checkCombatEnd
  -- return early if the player ended the fight or took a free action
  if not endOfPlayerTurn then enemyTurn else return ()

  endOfEnemyTurn <- checkCombatEnd
  return ()

enemyTurn :: State World ()
enemyTurn = do
  w <- get

  -- Fire weapon 0
  doAction $ Just (head $ (combatActions w (getEnemy w) player))

playerTurn :: Char -> State World ()
playerTurn keyPress = do
  w <- get
  doAction $ matchKey (availableActions w player (getEnemy w)) keyPress
  return ()
  where
    availableActions w = case (w.status) of
--      Combat ReloadPrompt -> weaponReloadActions w
      Combat _   -> combatActions w
      LootScreen -> lootActions w
      PathSelect -> pathActions w
      Tinker     -> tinkerActions w
