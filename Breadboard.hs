{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Breadboard where
import HCad
-- import HCad.Nuts
import Algebra.Linear ()
import Algebra.Classes hiding ((*<))
-- import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, (.))

import Common
import NiceNano


boardThickness = 1.61
boardLength = 45.72
boardWidth = 33.33
boardMountPointsDistToCenter = 18
boardMountHoleDiam :: R
boardMountHoleDiam = 3.5
boardMountPointsDistToEdge = boardLength / 2 - boardMountPointsDistToCenter
-- >>> boardMountPointsDistToEdge
-- 5.5

supportTopThickness = 1.6
supportSideThickness = 1.2
boardUndersideClearance = 8
supportTol = 1
supportTotalHeight = boardUndersideClearance + boardThickness --  + supportTopThickness + 4
supportMinSz = 6

boardHoles :: Part '[] V2 R
boardHoles = forget $  mirrored (V2 0 1) $ translate (V2 0 boardMountPointsDistToCenter) $ scale (boardMountHoleDiam + 0.2) $ circle 


boardShape :: R -> Part2
                     '[ '["right"], '["back"], '["left"], '["front"], '["northEast"],
                        '["northWest"], '["southWest"], '["southEast"]]
                     R
boardShape tol = rectangleWithRoundedCorners 5 $
                 (+pure tol) $
                 V2 boardWidth boardLength 

boardNegativeShape :: Part '[] V2 R
boardNegativeShape = forget (boardShape supportTol)

-- >>> breadboardMain

boardNegativeSpace :: Bool -> Part '[] V3 R
boardNegativeSpace side =
  union (unions [screwAccess, shiftedNinNegativeSpace, resetBtnAccess side]) $ 
  color' 0.1 (V3 0.0 0.8 0.8) $
  forget $
  translate (V3 0 0 (-boardThickness / 2)) $
  center nadir $
  extrude 6 $
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

pinDistance :: R
pinDistance = 0.1 * inch


boardAndNin :: Part '[] V3 R
boardAndNin = unions [board, translate ninShift nin]

ninShift :: Euclid V3' R
ninShift = V3 (pinDistance/2) 5 2 -- 2mm shift up is a hack (unknown cause for shift)

shiftedNinNegativeSpace :: Part3 '[] R
shiftedNinNegativeSpace = translate ninShift ninNegativeSpace

                      

breadboardMain :: IO ()
breadboardMain = do
  writeFile "board.scad" $ rndr $ unions
   [boardAndNin,
    boardNegativeSpace True
    -- boardSupport
   ]

-- >>> breadboardMain


boardAnchor :: V3 R
boardAnchor = negate (locPoint ((nadir |<- northWest) board0))

screwAccess :: Part '[] (Euclid V3') R
screwAccess =
  color' 0.1 (V3 0.8 0.0 0.8) $
  forget $ 
  translate (V3 0 0 (-(2 + boardThickness))) $
  center zenith $ 
  extrude 10 $
  mirrored (V2 0 1) $
  translate (V2 0 boardMountPointsDistToCenter) $ scale0 8 circle 

resetBtnAccess :: Bool -> Part '[] V3 R
resetBtnAccess side = 
  color' 0.1 (V3 0.8 0.0 0.8) $
  forget $ 
  center zenith $ 
  extrude 10 $
  translate (V2 ((if side then negate else id) (7 - boardWidth / 2)) (5.5 - boardLength / 2)) $
  scale0 6 circle


boardSupport :: Part3 '[] R
boardSupport = forget $
               -- translate (V3 0 0 (boardThickness/2 + supportTopThickness)) $
               -- on zenith (push 0.6 (mirrored (V2 0 1) $ translate (V2 0 boardMountPointsDistToCenter) $ metricNutProfile m3 0.4)) $
               center zenith $
               extrude supportTotalHeight $
               difference boardHoles $ -- holes for screws
               -- mirrored (V2 0 1) $
               -- union (translate (V2 0                                           (negate (-boardLength/2 + 3.5))) $
               --        rectangle (V2 (boardMountPointsDistToEdge + supportMinSz) (boardMountPointsDistToEdge + 8))
               --       ) $
               union (unions [translate ((*) <$> V2 d 1 <*>  (V2 (boardWidth /2) (boardLength / 2) - pure 5)    ) $ scale0 10 $ circle |
                              d <- [-1,1]]) $
               translate (V2 0 -- (-boardWidth /2 - supportSideThickness - supportTol)
                         (-boardLength/2 - supportSideThickness - supportTol)
                         ) $
               center south $
               rectangle
                 (V2 (boardMountPointsDistToEdge + supportMinSz) (boardMountPointsDistToEdge + 8))
                  -- (V2 (boardWidth/2 + supportMinSz) (boardMountPointsDistToEdge + supportMinSz))


