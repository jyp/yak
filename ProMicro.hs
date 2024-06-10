{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module ProMicro where
import HCad
import HCad.Nuts
import Algebra.Linear ()
import Algebra.Classes -- hiding ((*<))
-- import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.))

import Common

usbConnectorSz :: Euclid V2' R
usbConnectorSz = (V2 8 3) -- tolerance

boxExtraHeight :: R
boxExtraHeight = 7

shiftNegative :: Part xs V3 R -> Part xs V3 R
shiftNegative = translate ((boxHeight/2) *^ zAxis)

-- >>> proMicroMain
mcuNegative :: Part '[] V3 R
mcuNegative =
  shiftNegative $
  mcuShift $
  forget $
  on zenith (pull (2.6) $ rectangle $ (V2 pcbWidth pcbLen) - pure 1.5) $ -- room for components (and headers top)
  on south  (pull (mMaxThickness m3 + 0.5) $ metricNutProfile m3 0.6) $ -- fixation nut
  on south  (union $ extrude (mMaxThickness m3 + 3) $ metricNutProfile m3 0.6) $ -- fixation nut entry hole
  on south  (pull (10) $ scale (3.5 :: R) $ circle) $ -- fixation screw hole
  on nadir (pull (4+boxHeight) $ rectangle $ (V2 pcbWidth pcbLen) + pure tol) $ -- top opening
  on (north |<- zenith)
     (union $
       color (V3 0.7 0.7 0.7) $
       translate ((-tol/2::R) *^ zAxis) $ -- recenter to the pcb, not the pcb hole which is tol larger
       yOrientation $
       translate ((-1) *< zAxis) $
       center south $ -- relative to the reference rectangle
       union (center zenith $ extrude holeLen $ rectangleWithRoundedCorners 2 $ usbConnectorSz + pure 4) $
       extrude holeLen $ 
       union (rectangleWithRoundedCorners 1.5 $ usbConnectorSz + pure 1.5) $ -- usb hole
       rectangle usbConnectorSz -- for reference rectangle
     ) $
  scale' (pcbSize + pure tol) cube -- base size of the pcb + tolerance
  where holeLen = 20
        tol = 1.3

mcuPositive :: Part _ V3 R
mcuPositive =
  translate ((-boxHeight/2) *< zAxis) $
  mcuShift $
  translate positiveShift $
  scale' (pcbSize + mcuBaseExtr + boxHeight *< zAxis) cube

-- >>> proMicroMain

positiveShift :: Euclid V3' R
positiveShift =
   (V3 0 -- width
     (-2) -- len
     (3.1 + (boxHeight/2))) -- thickness

boxHeight :: R
boxHeight = 17

mcuBaseExtr :: Euclid V3' R
mcuBaseExtr = V3 5 10 8

mcuShift :: Part xs V3 R -> Part xs V3 R
mcuShift = translate $ (-0.5::R) *^ (pcbSize + mcuBaseExtr) +  negate positiveShift

-- >>> proMicroMain

mcuBase :: Part '[] V3 R
mcuBase =   forget $ difference mcuNegative mcuPositive

-- mcuBox :: Part '[] V3' R
-- mcuBox =
--   forget $
--   difference usbHole $
--   translate ((2.25::R) *^ zAxis) $ -- line up the usb hole
--   union support $
--   translate ((negate (verticalRoom + pcbThickness/2)) *^ zAxis) $ -- line up the pcb plate
--   difference (translate ((wallHeights) *^ zAxis)$ extrude 1.6 $ translate (V2 0 (-1)) $ rectangle $ doorSize) $
--   (on zenith $ pull wallHeights $ at south (difference (center south $ rectangle room)) $ base) $
--   extrude thickness $
--   rectangle baseSz
--   where thickness = 2
--         wallHeights = headerLen+verticalRoom+pcbThickness+boxExtraHeight
--         baseSz = room + V2 (2*thickness) thickness
--         pcbSz = V2 pcbWidth pcbLen
--         room = pcbSz + pure 2
--         doorSize = room + pure 1.5
--         base = rectangle baseSz
--         supportThickness = pcbThickness+0.3
--         support =
--           union (extrude supportThickness (supportProfile 0.5)) $
--           mirrored zAxis $ translate (((supportThickness/2)) *^ zAxis)$ center nadir $ extrude 1 (supportProfile (-0.5))
--         supportProfile tol = at south (difference (center south $ rectangle (pcbSz + pure tol))) $ rectangle room 
--         verticalRoom = 5
--         usbHole = yOrientation $
--           on nadir (union (center zenith $ extrude 2 $ rectangleWithRoundedCorners 2 (usbConnectorSz + pure 4)))  $
--           extrude (pcbLen+2*thickness+0.01) $ rectangleWithRoundedCorners 1.4 $ usbHoleSz
--         usbHoleSz@(V2 _ usbT) = usbConnectorSz + pure 0.5

-- >>> proMicroMain

pcbSize :: V3 R
pcbSize@(V3 pcbWidth pcbLen pcbThickness) = V3 18.25 33.5 1.6

proMicro :: Part '[] V3 R
proMicro =
  shiftNegative $
  mcuShift $
  forget $
  -- mirror zAxis $
  on nadir (union $
            center nadir $
            color (V3 0.5 0.5 0.5) $
            (extrude headerLen $ mirrored (V2 1 0) $
             translate (V2 7.5 1) $
             rectangle $
             V2 headerWidth (12*headerWidth))) $
  on (north |<- zenith)
     (union $
       color (V3 0.7 0.7 0.7) $
       yOrientation $
       translate ((-2.0::R) *^ zAxis) $
       center nadir $
       extrude 6 $
       center south $
       rectangleWithRoundedCorners 1.4 usbConnectorSz) $
  color (V3 0  0.5 0.5) $ 
  scale' pcbSize cube


headerLen :: R
headerLen = 17
headerWidth :: R
headerWidth = 30/12

proMicroMain :: IO ()
proMicroMain = do
  -- writeFile "mcu.scad" (rndr $ proMicro)
  writeFile "mcu.scad" (rndr ${- union proMicro-} mcuBase)



-- >>> proMicroMain
