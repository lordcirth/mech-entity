module BrickApp where

import qualified Brick        as B
import           Components
import qualified Graphics.Vty as V

type Name = ()

theMap :: B.AttrMap
theMap = B.attrMap V.defAttr [ warningText, errorText ]
  where
    warningText = (B.attrName "warning", B.fg V.yellow)
    errorText   = (B.attrName "error", B.fg V.red)


warn :: B.Widget Name -> B.Widget Name
warn widget = B.withAttr (B.attrName "warning") widget


err :: B.Widget Name -> B.Widget Name
err widget = B.withAttr (B.attrName "error") widget


brickApp :: B.App World () Name
brickApp = B.App {
  B.appDraw           = drawUI
  ,B.appChooseCursor  = B.neverShowCursor
  ,B.appHandleEvent   = handleEvent
  ,B.appStartEvent    = return
  ,B.appAttrMap       = const theMap
}
