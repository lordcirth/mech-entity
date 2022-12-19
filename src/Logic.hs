module Logic where

import qualified Brick                     as B
import           Components
import           Control.Monad.Trans.State (State, execState, get, put)
import           Data.Char                 (intToDigit)
import           Data.List.Index           (imap)
import qualified Data.Map.Strict           as Map
import qualified Graphics.Vty              as V
import           Util


handleEvent :: World -> B.BrickEvent Name () -> B.EventM Name (B.Next World)

-- Halt on ESC
handleEvent gameState (B.VtyEvent (V.EvKey (V.KEsc) _)) = B.halt gameState

handleEvent gameState (B.VtyEvent (V.EvKey (V.KChar keyPress) _)) =
  process combatTurn
    where
      process :: (Char -> State World ()) -> B.EventM Name (B.Next World)
      process stage = B.continue $ execState (stage keyPress) gameState

-- no-op for all other inputs
handleEvent w _                                         = B.continue w


generateActions :: World -> ID -> [Action]
generateActions w actor =
  mainActions w actor


combatTurn :: Char -> State World ()
combatTurn keyPress = do
  playerTurn keyPress
  endOfPlayerTurn <- updateStatus
  -- return early if the player ended the fight or took a free action
  --if endOfPlayerTurn == Combat EnemyTurn then enemyTurn else return ()

  endOfEnemyTurn <- updateStatus
  return ()


playerTurn :: Char -> State World ()
playerTurn keyPress = do
  gameState <- get
  doAction $ matchKey (availableActions gameState) keyPress
  where
    availableActions gs = case (gs.status) of
      --Combat ReloadPrompt -> weaponReloadActions gs
      Combat _            -> combatActions gs


mainActions :: World -> ID -> [Action]
mainActions w actor = imap (attackAction w actor) $ getWeapons w actor


attackAction :: World -> ID -> Int -> ID  -> Action
attackAction w actor key item = Action {
  effect  = undefined
  ,item   = Just item
  ,key    = intToDigit key
  ,name   = getName w item
}
