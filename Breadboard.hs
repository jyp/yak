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


boardShape :: V2 R -> Part2
                     '[ '["right"], '["back"], '["left"], '["front"], '["northEast"],
                        '["northWest"], '["southWest"], '["southEast"]]
                     R
boardShape tol = rectangleWithRoundedCorners 5 $
                 (+ tol) $
                 V2 boardWidth boardLength 

boardNegativeShape :: Part '[] V2 R
boardNegativeShape = forget $ (boardShape (pure supportTol))

-- >>> breadboardMain

boardNegativeSpace :: Part '[] V3 R
boardNegativeSpace =
  union (unions [screwAccess, shiftedNinNegativeSpace, resetBtnAccess]) $ 
  color' 0.1 (V3 0.0 0.8 0.8) $
  forget $
  translate (V3 0 0 (-boardThickness / 2)) $
  center nadir $
  on zenith (pull 3 (boardShape (V2 supportTol (-3)))) $
  extrude (boardThickness + 0.6) $
  boardNegativeShape


board0 :: Part3
                 '[ '["bottom"], '["top"], '["right"], '["back"], '["left"],
                    '["front"], '["northEast"], '["northWest"], '["southWest"],
                    '["southEast"]]
                 R
board0 = extrude boardThickness $
         difference boardHoles $
         boardShape (pure 0)

unit :: R
unit = 2.54

board :: Part '[] V3 R
board =forget $
  on zenith (union $ translate (V3 (-4.5*unit) (-8*unit) 0) $ jstPH2) $ 
  on nadir ((pull (7-1.6) $ mirror (V2 0 1) $ translate resetBtnLoc $ scale (6::R) $ square )) $ 
  color' 0.3 (V3 0.0 0.0 0.0) $ board0


-- >>> breadboardMain

pinDistance :: R
pinDistance = 0.1 * inch

jstPH2 :: Part3
              ['["bottom"], '["top"], '["right"], '["back"], '["left"],
               '["front"], '["northEast"], '["northWest"], '["southWest"],
               '["southEast"]]
              R
jstPH2 = center nadir $ extrude 6.1 $ rectangle (V2 5.5 6.0)

boardAndNin :: Part '[] V3 R
boardAndNin = unions [board, translate ninShift nin]

ninShift :: Euclid V3' R
ninShift = V3 (pinDistance/2) (2.5*pinDistance) (2 - 2.6) -- 2mm shift up is a hack (unknown cause for shift)

shiftedNinNegativeSpace :: Part3 '[] R
shiftedNinNegativeSpace = translate ninShift ninNegativeSpace

breadboardMain :: IO ()
breadboardMain = do
  writeFile "board.scad" $ rndr $ unions
   [boardAndNin
   , boardNegativeSpace
   , boardSupport
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
  -- mirrored (V2 0 1) $
  translate (V2 0 (-boardMountPointsDistToCenter)) $ scale0 6.6 circle 

resetBtnLoc = V2 ((negate) (7 - boardWidth / 2)) (5.5 - boardLength / 2)

resetBtnAccess :: Part '[] V3 R
resetBtnAccess = 
  color' 0.1 (V3 0.8 0.0 0.8) $
  forget $ 
  center zenith $ 
  extrude 10 $
  translate resetBtnLoc $
  scale0 2.5 circle

boardSupport :: Part3 '[] R
boardSupport = forget $
               difference (extrude (supportTotalHeight+1) boardHoles) $ -- holes for screws
               -- translate (V3 0 0 (boardThickness/2 + supportTopThickness)) $
               -- on zenith (push 0.6 (mirrored (V2 0 1) $ translate (V2 0 boardMountPointsDistToCenter) $ metricNutProfile m3 0.4)) $
               union (translate (V3 0 (-boardMountPointsDistToCenter) (extraSupportHeight - supportTotalHeight)) $
                      center zenith $
                      extrude extraSupportHeight $
                      scale (9 :: R) circle
                     )$
               center zenith $
               extrude supportTotalHeight $
               -- mirrored (V2 0 1) $
               -- union (translate (V2 0                                           (negate (-boardLength/2 + 3.5))) $
               --        rectangle (V2 (boardMountPointsDistToEdge + supportMinSz) (boardMountPointsDistToEdge + 8))
               --       ) $
               union (unions [translate ((*) <$> V2 d 1 <*>  (V2 (boardWidth /2) (boardLength / 2) - pure 5)    ) $ scale0 10 $ circle |
                              d <- [-1,1]]) $
               translate (V2 0 (-boardLength/2 - supportSideThickness - supportTol)) $
               center south $
               rectangle
                 (V2 (boardMountPointsDistToEdge + supportMinSz - 4) (boardMountPointsDistToEdge + 10))
  where extraSupportHeight = 7

-- >>> breadboardMain
