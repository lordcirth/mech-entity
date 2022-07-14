module Logic where

import qualified Brick      as B
import           Components

handleEvent :: World -> B.BrickEvent Name () -> B.EventM Name (B.Next World)
handleEvent w _ = B.continue w
