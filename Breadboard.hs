{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Breadboard where
import HCad
import HCad.Nuts
import Algebra.Linear ()
import Algebra.Classes hiding ((*<))
-- import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.))

import Common
import NiceNano


boardThickness = 1.61
boardLength = 45.72
boardWidth = 33.33
boardMountPointsDistToCenter = 18
boardMountHoleDiam = 3.5
boardMountPointsDistToEdge = boardLength / 2 - boardMountPointsDistToCenter
-- >>> boardMountPointsDistToEdge
-- 5.5

supportTopThickness = 1.6
supportSideThickness = 1.2
boardUndersideClearance = 7
supportTol = 0.8
supportTotalHeight = boardUndersideClearance + boardThickness + supportTopThickness + 4
supportMinSz = 6

boardHoles :: Part '[] V2 R
boardHoles = forget $  mirrored (V2 0 1) $ translate (V2 0 boardMountPointsDistToCenter) $ scale (boardMountHoleDiam + 0.2) $ circle 


boardShape :: R
                -> Part2
                     '[ '["right"], '["back"], '["left"], '["front"], '["northEast"],
                        '["northWest"], '["southWest"], '["southEast"]]
                     R
boardShape tol = rectangleWithRoundedCorners 7 $
                 (+pure tol) $
                 V2 boardWidth boardLength 

boardNegativeShape :: Part '[] V2 R
boardNegativeShape = forget (boardShape supportTol)

-- >>> breadboardMain

boardNegativeSpace :: Part '[] V3 R
boardNegativeSpace =
  color' 0.1 (V3 0.0 0.8 0.8) $
  forget $
  translate (V3 0 0 (-boardThickness / 2)) $
  center zenith $
  extrude 20 $
  boardNegativeShape

board0 :: Part3
                 '[ '["bottom"], '["top"], '["right"], '["back"], '["left"],
                    '["front"], '["northEast"], '["northWest"], '["southWest"],
                    '["southEast"]]
                 R
board0 = extrude boardThickness $
         difference boardHoles $
         boardShape 0

board :: Part '[] V3 R
board = color' 0.3 (V3 0.0 0.0 0.0) $ forget board0 

boardAndNin :: Part '[] V3 R
boardAndNin = unions [board, translate (V3 0 5 0) nin]

breadboardMain :: IO ()
breadboardMain = do
  writeFile "board.scad" $ rndr $ unions
   [boardAndNin,
    usbcConnectorNegativeSpace
    -- boardNegativeSpace
   , boardSupport
   ]

-- >>> breadboardMain


boardAnchor :: V3 R
boardAnchor = negate (locPoint ((nadir |<- northWest) (board0)))


boardSupport :: Part3 '[] R
boardSupport = forget $
               translate (V3 0 0 (boardThickness/2 + supportTopThickness)) $
               on zenith (push 0.6 (mirrored (V2 0 1) $ translate (V2 0 boardMountPointsDistToCenter) $ metricNutProfile m3 0.4)) $
               center zenith $
               extrude supportTotalHeight $
               difference boardHoles $ -- holes for screws
               -- mirrored (V2 0 1) $
               union (translate (V2 0 (negate (-boardLength/2 + 1))) $
                      rectangle (V2 (boardMountPointsDistToEdge + supportMinSz) (boardMountPointsDistToEdge + 10))) $ 
               translate (V2 (-boardWidth /2 - supportSideThickness - supportTol)
                             (-boardLength/2 - supportSideThickness - supportTol)) $
               center southWest $
               rectangle (V2 (boardWidth/2 + supportMinSz) (boardMountPointsDistToEdge + supportMinSz))


