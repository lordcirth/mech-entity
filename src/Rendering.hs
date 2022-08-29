module Rendering where

import qualified Brick                as B
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Components
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust)
import           InitialState
import           Util

drawUI :: World -> [B.Widget Name]
drawUI w

  | Combat _ <- s =
    drawCombatUI w

--  | LootScreen <- w.status =
--    -- drawLootScreen w
--    undefined
--
--  | Tinker <- w.status =
--    undefined
--    -- drawTinkerScreen w
--
--  | PathSelect <- w.status =
--    undefined
    -- drawPathSelect w

  | GameOver <- s =
    drawGameOver w

  where
    s = w.status


drawCombatUI :: World -> [B.Widget Name]
drawCombatUI w = [
  playerBars w B.<+> (hCenter $ B.str "|") B.<+> enemyBars
  B.<=> menu
  B.<=> B.padTop B.Max eventList
  ]
  where
    -- drawCombatUI is only run if an enemy exists
    playerBars = drawUnitInfo w player
    enemyBars = drawUnitInfo w maintDrone
    actionList = box 10 80 "Actions" $ drawActions (combatActions w)
--    reloadList = box 10 80 "Reload which weapon?" $ drawActions (weaponReloadActions w)
    eventList = box 12 200 "Event Log" $
      B.padTop B.Max $ B.padRight B.Max $ B.str $
      unlines $ reverse $ take 10 $ w.eventLog
    menu = case (w.status) of
--      Combat ReloadPrompt -> reloadList
      Combat _            -> actionList

box :: Int -> Int -> String -> B.Widget Name -> B.Widget Name
box v h title widget = B.vLimit v $ B.hLimit h $ borderWithLabel (B.str title) widget


-- Output a fixed-length string version of a number
padInt :: Int -> Int -> String
padInt len num = replicate pad ' ' ++ show num
  where
    pad = len - (length $ show num)


drawBar :: String -> Int -> Int -> B.Widget Name
drawBar barName current maxSize = B.padRight (B.Pad 3) $ B.str raw
  where
    raw = barName ++ ": " ++ padInt 3 current ++ " / " ++ padInt 3 maxSize

drawUnitInfo :: World -> ID -> [B.Widget Name]
drawUnitInfo w u = show unitInfo
  where
    unitInfo = fromJust $ Map.lookup u (w.unit)

drawActions :: World -> [B.Widget Name]
drawActions _ = B.str "foo"

drawGameOver :: World -> [B.Widget Name]
drawGameOver _ = [hCenter $ vCenter $ box 3 11 "" $ B.str $ "GAME OVER"]
