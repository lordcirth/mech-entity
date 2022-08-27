module Rendering where

import qualified Brick                as B
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Components

drawUI :: World -> [B.Widget Name]
drawUI w

  | Combat <- w.status =
    drawCombatUI w

  | LootScreen <- w.status =
    drawLootScreen w

  | Tinker <- w.status =
    drawTinkerScreen w

  | PathSelect <- w.status =
    drawPathSelect w

  | GameOver <- w.status =
    drawGameOver w


drawCombatUI :: GameState -> [B.Widget Name]
drawCombatUI gameState = [
  playerBars gameState B.<+> (hCenter $ B.str "|") B.<+> enemyBars
  B.<=> menu
  B.<=> B.padTop B.Max eventList
  ]
  where
    -- drawCombatUI is only run if an enemy exists
    enemy = fromJust $ getEnemy gameState
    enemyBars = drawUnitInfo enemy.unitInfo
    actionList = box 10 80 "Actions" $ drawActions (combatActions gameState)
    reloadList = box 10 80 "Reload which weapon?" $ drawActions (weaponReloadActions gameState)
    eventList = box 12 200 "Event Log" $
      B.padTop B.Max $ B.padRight B.Max $ B.str $
      unlines $ reverse $ take 10 $ gameState.eventLog
    menu = case (gameState.status) of
      Combat ReloadPrompt -> reloadList
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


drawGameOver :: World -> [B.Widget Name]
drawGameOver _ = [hCenter $ vCenter $ box 3 11 "" $ B.str $ "GAME OVER"]
