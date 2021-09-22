{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module NiceNano where
import HCad
import HCad.Nuts
import Algebra.Linear ()
import Algebra.Classes hiding ((*<))
-- import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.))

import Common

usbConnectorThickness = 2.6

usbConnectorSz :: Euclid V2' R
usbConnectorSz = (V2 8.4 usbConnectorThickness)

boxExtraHeight :: R
boxExtraHeight = 7

shiftNegative :: Part xs V3 R -> Part xs V3 R
shiftNegative = translate ((boxHeight/2) *< zAxis)

-- >>> ninMain

mcuPositive :: Part _ V3 R
mcuPositive =
  translate ((-boxHeight/2) *< zAxis) $
  scale' (pcbSize + mcuBaseExtr + boxHeight *< zAxis) cube

-- >>> ninMain

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

-- >>> ninMain


-- >>> ninMain

pcbSize :: V3 R
pcbSize@(V3 pcbWidth pcbLen pcbThickness) = V3 18.25 33.5 1.6


nin0 =
  translate (V3 0 0 (-headerHeight)) $
  center zenith $
  on (zenith |<- north)
     (union $
       color (V3 0.7 0.7 0.7) $
       translate ((-2.0::R) *^ zAxis) $
       center nadir $
       extrude 6 $
       translate (V2 0 (usbConnectorThickness/2)) $
       rectangleWithRoundedCorners 1 usbConnectorSz) $
  color (V3 0  0.5 0.5) $ 
  scale' pcbSize cube

nin :: Part '[] (Euclid V3') Double
nin = forget nin0

usbcConnectorNegativeSpace =
  forget $
  color' 0.1 (V3 0.8 0.0 0.8) $
  withLoc ((zenith |<- north) nin0) $
  translate ((-2.0::R) *^ zAxis) $
  center nadir $ 
  extrude 20 $
  translate (V2 0 (usbConnectorThickness/2)) $
  rectangleWithRoundedCorners 1 (usbConnectorSz + pure 2)


ninMain :: IO ()
ninMain = do
  -- writeFile "mcu.scad" (rndr $ proMicro)
  writeFile "mcu.scad" (rndr $ unions [nin, usbcConnectorNegativeSpace])

headerHeight :: Double
headerHeight = 0.122 * inch


-- >>> ninMain
