module Logic where

import qualified Brick           as B
import           Components
import           Data.Char       (intToDigit)
import           Data.List.Index (imap)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import qualified Graphics.Vty    as V
import           Util

handleEvent :: World -> B.BrickEvent Name () -> B.EventM Name (B.Next World)

-- Halt on ESC
handleEvent gameState (B.VtyEvent (V.EvKey (V.KEsc) _)) = B.halt gameState

-- no-op for all other inputs
handleEvent w _                                         = B.continue w

generateActions :: World -> ID -> [Action]
generateActions w actor =
  mainActions w actor


mainActions :: World -> ID -> [Action]
mainActions w actor = imap (attackAction w actor) $ getWeapons w actor


attackAction :: World -> ID -> Int -> ID  -> Action
attackAction w actor key item = Action {
  effect  = undefined
  ,item   = Just item
  ,key    = intToDigit key
  ,name   = (fromJust $ Map.lookup item w.meta).name
}
