{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module NiceNano (nin, ninNegativeSpace) where
import HCad
import Algebra.Linear ()
import Algebra.Classes
-- import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.))



import Common

-- About 18mm wide, 34.2mm in length with USB-C, 33.4 without USB-C, 3.2mm thick

-- usbConnectorWidthMale :: R
-- usbConnectorWidthMale = 8.4
usbConnectorWidthFemale :: R
usbConnectorWidthFemale = 8.63 + 2* 0.57

-- usbConnectorThicknessMale :: R
-- usbConnectorThicknessMale = 2.6
usbConnectorThicknessFemale :: R
usbConnectorThicknessFemale = 3.16

usbCableSz :: V2 R
usbCableSz = V2 9.5 4 -- pure (2*usbConnectorPlasticThicknessPlusTol) + V2 usbConnectorWidthMale usbConnectorThicknessMale

-- >>> usbCableSz :: V2 R
-- Euclid {fromEuclid = VNext (VNext VZero 13.4) 8.6}

usbConnectorSz :: Euclid V2' R
usbConnectorSz = V2 usbConnectorWidthFemale usbConnectorThicknessFemale

boxExtraHeight :: R
boxExtraHeight = 7

shiftNegative :: Part xs V3 R -> Part xs V3 R
shiftNegative = translate ((boxHeight/2) *< zAxis)

-- >>> ninMain

mcuPositive :: Part '[ '["bottom"], '["top"], '["right"], '["back"],
                     '["left"], '["front"], '["northEast"], '["northWest"],
                     '["southWest"],
                     '["southEast"]] V3 R
mcuPositive =
  translate ((-boxHeight/2) *< zAxis) $
  scale' (pcbSize + mcuBaseExtr + boxHeight *< zAxis) cube

-- >>> ninMain

positiveShift :: Euclid V3' R
positiveShift =
   V3 0 -- width
      (-2) -- len
      (3.1 + (boxHeight/2)) -- thickness

boxHeight :: R
boxHeight = 17

mcuBaseExtr :: Euclid V3' R
mcuBaseExtr = V3 5 10 8

mcuShift :: Part xs V3 R -> Part xs V3 R
mcuShift = translate $ (-0.5::R) *^ (pcbSize + mcuBaseExtr) +  negate positiveShift

-- >>> ninMain



pcbSize :: V3 R
pcbWidth :: R
pcbLen :: R
pcbThickness :: R
pcbSize@(V3 pcbWidth pcbLen pcbThickness) = V3 18 33.4 1.62


nin0 =
  translate (V3 0 0 (-headerHeight)) $
  center zenith $
  on (zenith |<- north)
     (union $
       color (V3 0.7 0.7 0.7) $
       translate ((1.2::R) *^ zAxis) $
       center zenith $
       extrude 6 $
       translate (V2 0 (usbConnectorThicknessFemale/2)) $
       rectangleWithRoundedCorners 1 usbConnectorSz) $
  color (V3 0  0.5 0.5) $ 
  scale' pcbSize cube

nin :: Part '[] (Euclid V3') Double
nin = forget nin0

usbcConnectorNegativeSpace :: Part '[] V3 Double
usbcConnectorNegativeSpace =
  forget $
  color' 0.1 (V3 0.8 0.0 0.8) $
  withLoc ((zenith |<- north) nin0) $
  translate ((-3.0::R) *^ zAxis) $
  center nadir $ 
  extrude 20 $
  translate (V2 0 (usbConnectorThicknessFemale/2)) $
  rectangleWithRoundedCorners 2 usbCableSz

-- | where the nicenano will go
mainNegativeSpace :: Part '[] V3 R
mainNegativeSpace = forget $
  color' 0.1 (V3 0.8 0.0 0.8) $
  center zenith $ 
  extrude 6 $
  rectangle (V2 (pcbWidth + 3.5) (pcbLen + 5))

-- clear path to see leds
ledsNegativeSpace :: Part '[] V3 R
ledsNegativeSpace = forget $
  color' 0.1 (V3 0.8 0.0 0.8) $
  center zenith $
  extrude 15 $
  mirrored (V2 1 0) $ 
  translate (V2 6 7) $
  circle

ninNegativeSpace :: Part '[] V3 Double
ninNegativeSpace = unions [usbcConnectorNegativeSpace,mainNegativeSpace,ledsNegativeSpace]

usbConnectorPlasticThicknessPlusTol :: R
usbConnectorPlasticThicknessPlusTol = 2.5

ninMain :: IO ()
ninMain = do
  -- writeFile "mcu.scad" (rndr $ proMicro)
  writeFile "mcu.scad" (rndr $ unions [nin, ninNegativeSpace])

headerHeight :: Double
headerHeight = 0.122 * inch

-- >>> headerHeight
-- 3.0987999999999998

-- >>> ninMain
