module Rendering where

import qualified Brick               as B
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Components

drawUI :: World -> [B.Widget Name]
drawUI w = drawGameOver w
  

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
