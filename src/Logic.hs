module Logic where

import qualified Brick        as B
import           Components
import qualified Graphics.Vty as V

handleEvent :: World -> B.BrickEvent Name () -> B.EventM Name (B.Next World)

-- Halt on ESC
handleEvent gameState (B.VtyEvent (V.EvKey (V.KEsc) _)) = B.halt gameState

-- no-op for all other inputs
handleEvent w _                                         = B.continue w

generateActions :: World -> ID -> [Action]
generateActions w actor =
  mainActions w actor


mainActions :: World -> ID -> [Action]
mainActions w actor = []
