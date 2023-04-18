module Rendering where

import qualified Brick                as B
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Components
import           Data.Char            (intToDigit)
import           Data.List.Index      (imap)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust)
import           InitialState
import           Util

drawUI :: World -> [B.Widget Name]
drawUI w

  | Combat _ <- s =
    drawCombatUI w

  | LootScreen <- w.status =
    drawLootScreen w

  | Tinker <- w.status =
    drawTinkerScreen w
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
  playerBars B.<+> (hCenter $ B.str "|") B.<+> enemyBars
  B.<=> menu
  B.<=> B.padTop B.Max (drawEventList w)
  ]
  where
    -- drawCombatUI is only run if an enemy exists
    playerBars = drawUnitInfo w player
    enemyBars = drawUnitInfo w (getEnemy w)
    actionList = box 10 80 "Actions" $ drawActions w $ combatActions w player (getEnemy w)
--    reloadList = box 10 80 "Reload which weapon?" $ drawActions (weaponReloadActions w)
    menu = case (w.status) of
--      Combat ReloadPrompt -> reloadList
      Combat _            -> actionList

drawLootScreen :: World -> [B.Widget Name]
drawLootScreen w = [
  playerBars
  B.<=> menu
  B.<=> B.padTop B.Max (drawEventList w)
  ]
  where
    playerBars = drawUnitInfo w player
    menu = box 10 80 "Loot" $ drawActions w $ lootActions w player (getEnemy w)

drawTinkerScreen :: World -> [B.Widget Name]
drawTinkerScreen w = [
  playerBars
  B.<=> menu
  B.<=> B.padTop B.Max (drawEventList w)
  ]
  where
    playerBars = drawUnitInfo w player
    menu = box 10 80 "Tinkering" $ drawActions w $ []


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


drawUnitInfo :: World -> ID -> B.Widget Name
drawUnitInfo w u =
  drawBar "Armor" (unitInfo.armor) (unitInfo.maxArmor)
  B.<=>
  drawBar "Health" (equipInfo.hp) (equipInfo.maxHp)
  where
    unitInfo = fromJust $ Map.lookup u (w.unit)
    equipInfo = fromJust $ Map.lookup u (w.equip)


drawEventList :: World -> B.Widget Name
drawEventList w = box 12 200 "Event Log" $
      B.padTop B.Max $ B.padRight B.Max $ B.str $
      unlines $ reverse $ take 10 $ w.eventLog


drawActions :: World -> [Action] -> B.Widget Name
drawActions w actions = actionList
  where
    actionList :: B.Widget Name
    actionList = (B.vBox $ map (renderAction w) actions)


renderAction :: World -> Action -> B.Widget Name
renderAction w action = header B.<+> (B.str $ action.name)
  where
    header = B.str $ action.key : " | "


drawGameOver :: World -> [B.Widget Name]
drawGameOver w = [
    hCenter $ vCenter $ box 3 11 "" $ B.str $ "GAME OVER",
    drawEventList w
    ]
