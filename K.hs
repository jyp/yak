{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

module K where

import HCad
import Algebra.Linear ()
import Algebra.Classes
import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.))
import qualified Prelude
import Common
import NiceNano
import Breadboard

data KeyType = CherryMX -- https://datasheet.octopart.com/MX1A-11NW-Cherry-datasheet-34676.pdf
             | KailhChoc -- https://github.com/keyboardio/keyswitch_documentation/blob/master/datasheets/Kailh/CPG135001D03rev1-ChocWhite.pdf
             | ChocV2 -- https://www.kailhswitch.com/uploads/202015927/PG135301D03.pdf?rnd=379
             deriving Eq

keyType :: KeyType
keyType = KailhChoc

-- >>> main

keycapTopSize :: V2 R
keycapTopSize = case keyType of
  KailhChoc -> V2 keycapWidth keycapLen - pure 2
  _ -> pure (0.5 * inch) -- dsa keycap

keycapTolerance :: R
keycapTolerance = 0.10

keycapWidth :: R
keycapWidth = case keyType of
  KailhChoc -> 17.5 + keycapTolerance -- https://mkultra.click/mbk-choc-keycaps (also measured)
  _ -> 18.12 {- actual-} + keycapTolerance
    -- 0.725 * inch -- reported (18.415 mm)

keycapLen :: R
keycapLen = case keyType of
  KailhChoc -> 16.5 + keycapTolerance -- https://mkultra.click/mbk-choc-keycaps (also measured)
  _ -> keycapWidth


dsa125Width :: R
dsa125Width = (23.11 + keycapTolerance)

-- >>> keycapWidth
-- 18.22

-- How much the keycap sticks out over the stem
keycapTopThickness :: Double
keycapTopThickness = case keyType of
  CherryMX -> 4.5 -- mm 
  KailhChoc -> 1.36 -- https://mkultra.click/mbk-choc-keycaps
  ChocV2 -> 2.7 -- setup in KeyV2 repo, branch choc2

keycapZ :: R
keycapZ = case keyType of
  CherryMX -> 0.291 * inch
  KailhChoc -> 2.86 --- CHECK ?
  ChocV2 -> stemHeight + keycapTopThickness --- Print keycaps for that!

-- >>> keycapZ
-- 2.86

advertisedSwitchSize :: V2 R
advertisedSwitchSize = pure $ (0.551 + 0.002 + 0.1) * inch

-- >>> advertisedSwitchSize
-- Euclid {fromEuclid = VNext (VNext VZero 16.586199999999998) 16.586199999999998}

switchSize :: V2 Double
-- switchSize = pure (14 - 0.1) -- trim this down a hair for a tighter fit -- that's suitabe for primaselect silver
switchSize = case keyType of
  CherryMX -> pure (14 - 0.1)  -- transparent filament; 14 for prima easyprint
  ChocV2 -> pure (14 - 0.1)
  KailhChoc -> V2 14 13.8 - pure 0.1 -- measured

stemHeight :: Double
stemHeight = case keyType of
               CherryMX -> 3.6
               KailhChoc -> 3.00
               ChocV2 -> 3.3

keyTravel :: Double
keyTravel = case keyType of
  CherryMX -> stemHeight
  KailhChoc -> stemHeight
  ChocV2 -> stemHeight

-- height of the case of the switch (excluding pins and stem)
switchHeight :: Double
switchHeight = case keyType of
  CherryMX -> 11.6
  KailhChoc -> 5.00
  ChocV2 -> 5.30
    
switchDepthBelowPlate :: Double
switchDepthBelowPlate = case keyType of
  CherryMX -> 5
  KailhChoc -> 2.20 -- switchHeight - (5.8 - 3)
  ChocV2 -> 2.20

keycapTopToPlate :: R
keycapTopToPlate = keycapTopThickness + stemHeight + switchHeight - switchDepthBelowPlate

-- >>> keycapTopToPlate
-- 7.8999999999999995

switchPinLen :: R
switchPinLen = case keyType of
  KailhChoc -> 2.85
  _ -> 3.3

switchPinDiameter :: R
switchPinDiameter = case keyType of
  KailhChoc -> 3.3
  _ -> 4

necessaryDepthBelowPlate :: R
necessaryDepthBelowPlate = switchDepthBelowPlate + switchPinLen


-- >>> main
type ModelPoints = '[ '["bottom"], '["top"], '["right"], '["back"], '["left"],
                         '["front"], '["northEast"], '["northWest"], '["southWest"],
                         '["southEast"]] 

frameThickness :: R
frameThickness = case keyType of
  KailhChoc -> 3 -- otherwise the pins hardly stick out and seems really hard to solder (?)
  _ -> 4

-- | A base square of thickness t and footprint w, 
base :: R -> V2 R -> Part ModelPoints V3 R
base t w =
  translate (V3 0 0 (-keycapTopToPlate)) $
  center zenith $
  extrude t $
  rectangle w
  
-- base squares, to be connected
baseModel :: Int -> Int -> Part3 ModelPoints R
baseModel i j = base frameThickness (keycapSize i j)


extraFramingHeight :: R
extraFramingHeight = 1

keycapPressedDistanceToTopPlate :: Double
keycapPressedDistanceToTopPlate =
  case keyType of
    CherryMX -> 1.75
    KailhChoc -> 2
    ChocV2 -> 0.5

-- >>> main
mountNegative :: Int -> Int -> Part '[] V3 R
mountNegative i j =
  forget $
  union (translate (V3 0 0 (-keycapTopToPlate)) $ center nadir $ extrude 2 $ rectangle (switchSize + pure 1.5)) $ -- form the switch rest plate as a flat surface
  translate (V3 0 0 (-keycapTopToPlate+keycapPressedDistanceToTopPlate-0.25)) $ --
  union (extrude 34 $ rectangle switchSize) $ -- make sure the switch has no hindrance in either direction, so it can be mounted and worked on.
  center nadir $ -- upwards of the keycap
  extrude 20 $ -- clear this much
  rectangle $
  (keycapSize i j + pure 1.5) -- room for pushing keycap, with some tolerance

mountSize :: Int -> Int -> V2 R
mountSize i j = keycapSize i j + 2 *< V2 columnSep rowSep


mountModel :: Int -> Int -> Part ModelPoints V3 R
mountModel i j =
  color (V3 1 0 0) $
  -- forget $
  translate (V3 0 0 (-keycapTopToPlate)) $
  center zenith $
  extrude frameThickness $
  -- difference (forget (rectangle switchSize))
  rectangle (mountSize i j)

switchModel :: Part '[] V3 R
switchModel =
  color (V3 0.5 0.5 1) $
  forget $
  translate ((-keycapTopThickness-stemHeight+0.01::R) *^ zAxis) $
  (union $ translate ((switchDepthBelowPlate - switchHeight) *< zAxis) $  unions
     [forget $ center nadir $ extrude 1 (scale 16 square),
      truncatedPyramid switchDepthBelowPlate (pure 12) (pure 14),
      forget $ translate ((-switchDepthBelowPlate) *< zAxis) $ center zenith $
       extrude switchPinLen
       $ scale switchPinDiameter circle]) $
  truncatedPyramid (switchHeight - switchDepthBelowPlate) (pure 16) (pure 12)


-- >>> main

-- | Approximation of keycap model for debugging
keycapModel :: Int -> Int -> Part '[] V3 R
keycapModel i j = keycapModel' (keycapSize i j) where
  keycapModel' :: V2 R -> Part '[] V3 R
  keycapModel' sz = color (V3 0.5 0.5 0.5) $
                    (union $  truncatedPyramid columnReferenceZ sz topSize) $
                    forget $
                    translate (V3 0 0 (-columnReferenceZ)) $
                    center zenith $
                    extrude (keycapZ-columnReferenceZ) (rectangle sz)
    where taper = V2 keycapWidth keycapLen - keycapTopSize
          topSize = sz - taper

keycapLonger :: Int -> Int -> Bool
keycapLonger _ _ | keyType == KailhChoc = False
keycapLonger i j | j == (homeRow-2) && i <= 0 = True 
keycapLonger _ _ = False

keycapSize :: Int -> Int -> V2 R
keycapSize i j = V2 keycapWidth $ case keycapLonger i j of
    True -> dsa125Width
    False -> keycapLen


intrinsicPitch :: R -> Part xs2 V3 R -> Part xs2 V3 R
intrinsicPitch angle = relativeTo (V3 0 (getXin2 keycapTopSize / 2) 0) (rotate3d angle xAxis)

circularColumnOf :: R -> R -> R -> R -> (Int -> Part3 xs R) -> Int -> Part xs V3 R
circularColumnOf basePitch offset width r part i = t (part i)
  where theta = 2*sin (width/(r+offset)/2) -- how much angle each part takes (according to width)
        pitch = basePitch + (i-alignmentRow) *^ theta
        t = relativeTo (V3 0 0 r)
            (rotate3d pitch xAxis)

fan :: Functor f => V3 R -> V3 R -> R -> R -> (Int -> f (Option (Part xs V3 R))) -> R -> Int -> f (Option (Part xs V3 R))
fan depthAxis rotAxis offset width parts r n = map2 t (parts n)
  where pitch :: R
        pitch = 2*sin (width/(r+offset)/2)
        t = rotate (rotation3d  (n*^pitch) rotAxis) .
            translate ((-r) *^ depthAxis)

-- | how much below the finger resting position is the longest keycap Y length?
columnReferenceZ :: R
columnReferenceZ = case keyType of
                     KailhChoc -> 0.4 -- actually, mbk keycaps
                     _ -> keycapZ

circularColumnLargeRadius :: R -> R -> (Int -> Part3 xs R) -> Int -> Part xs V3 R
circularColumnLargeRadius sep = circularColumnOf 0 columnReferenceZ (keycapLen + sep)

-- circularColumnSmallRadius :: R -> R -> (Int -> Part3 xs R) -> Int -> Part xs V3 R
-- circularColumnSmallRadius sep = circularColumnOf 0 0 (getXin2 keycapTopSize + sep)

circularColumn :: Double -> (Int -> Part3 xs R) -> Int  -> Part xs V3 R
circularColumn r = circularColumnLargeRadius rowSep r

limit :: Int -> Int -> (Int -> a) -> Int -> Option a
limit i j f k | i <= k && k <= j = Yes (f k)
              | True = None

homeRow :: Int
homeRow = 0

columnSep :: R
columnSep = case keyType of
  KailhChoc -> 0.5
  _ -> 0.75

thumbColumnSep :: R
thumbColumnSep = 0.75

rowSep :: R
rowSep = case keyType of
  KailhChoc -> 0.6
  _ -> 0.75

map2 :: (Functor f1, Functor f2) =>
              (a -> b) -> f1 (f2 a) -> f1 (f2 b)
map2 f = fmap (fmap f)

map3 :: (Functor f1, Functor f2, Functor f) =>
              (a -> b) -> f1 (f2 (f a)) -> f1 (f2 (f b))
map3 f = map2 (fmap f)

alignmentRow :: Int
alignmentRow = 2 -- row where fingers will align perfectly, if there was no offset.

col0ofs :: V3 R
col0ofs = case keyType of
  KailhChoc -> V3 (-0.5) 3 0
  _ -> V3 3 3 0

col0Yaw :: Int -> R
col0Yaw j = (-2-3*fromIntegral j)*degree

-- >>> main
fingers :: (Int -> Int -> Part3 xs R) -> Int -> Int -> Option (Part xs V3 R)
fingers kModel i = 
  rowBounds $
  fmap (translate (V3 0 (-ofs i) 0) . -- column staggering
        rotate3d (30*degree) xAxis . -- 4 fingers pitch
        translate (V3 (i *^ (keycapWidth+columnSep)) 0 0)) $
  case i of
   0 -> fmap (translate col0ofs) $
            circularColumnOf (-2*degree) keycapZ (keycapWidth+rowSep+1.5) (fRadius i) $
            (\j ->
               rotate3d (col0Yaw j) zAxis $
               relativeTo (V3 (getXin2 keycapTopSize / 2) 0 0) (rotate3d 0.4 yAxis) $ -- rotate towards the middle of the hand
               ki j)
            -- extra index
   _ -> circularColumn (fRadius i) ki -- four fingers
  where fRadius 0 = fRadius 1
        fRadius 5 = fRadius 4+1
        fRadius j = fLen j - keyTravel - 20
        fLen :: Int -> R
        fLen 0 = fLen 1
        fLen 1 = 73
        fLen 2 = 83
        fLen 3 = 75
        fLen 4 = 60
        fLen 5 = fLen 4
        fLen _ = error "only 4 fingers"
        rowBounds | i < 0 || i > 5 = const (const None)
                  | i == 0 = limit (homeRow-1) (homeRow+2)
                  | i == 2 = limit (homeRow-3) (homeRow+2) -- wiring is annoying
                  | i >= 4 = limit (homeRow-1) (homeRow+2)
                  | otherwise = limit (homeRow-2) (homeRow+2)
        ofs i = (fLen 4 - fLen i)
        ki = \j -> intrinsicPitch (-iPitch j) (kModel i j)
        iPitch _ =  7*degree

-- >>> main

-- | Locate a finger (key) position by index.
fingerLoc :: ('[Zenith] ∈ xs) => ((Int -> Int -> Part3 ModelPoints R) -> Int -> Int -> Option (Part3 xs R)) -> Int -> Int -> V3 R
fingerLoc f column row = locPoint (zenith (fromYes (f (const $ const $ scale 0.1 cube) column (homeRow+row)))) - (0.5 *< z0 (keycapSize column row))

-- >>> main
thumb, thumb' :: (Int -> Int -> Part3 xs R) -> Int -> Int -> Option (Part xs V3 R)
thumb kModel i j 
  | i < 0 = None
  | i > 2 = None
  | j < 0 = None
  | j > 1 = None
  | i == 0 && j == 1 = None
  | otherwise = Yes $
    translate (homeIndexPoint + offset) $ -- cluster offset
    rotate3d (-15*degree) yAxis $ -- cluster roll
    rotate3d (15*degree) xAxis $  -- cluster pitch
    rotate3d (15*degree) zAxis $  -- cluster yaw
    (rotate3d ((7*i)*^degree) zAxis) $ -- column yaw
    translate (((-i)*^(keycapWidth+thumbColumnSep) - 4) *^ xAxis) $ -- column offset (full)
    (rotate3d ((5*i)*^degree) yAxis) $ -- column roll
    translate (j*^ V3 2 (dsa125Width/2 + keycapLen/2 - 2.5) 0 ) $ -- row offset
    relativeTo (V3 0 (-keycapLen/2) 0) (rotate3d ((j*^40)*degree) xAxis) $ -- row pitch
    translate (thumb1stRowOffset i j) $
    kModel i j
    where homeIndexPoint = fingerLoc fingers 1 0
          offset = case keyType of
            CherryMX -> V3 (-10) (-42) 17
            ChocV2 -> V3 (-9) (-41) 17
            KailhChoc -> V3 (-8) (-40) 17


thumb1stRowOffset :: Int -> Int -> V3 R
thumb1stRowOffset 1 1 | keyType == KailhChoc = V3 (6) (0.5) 0
thumb1stRowOffset _ 1 | keyType == KailhChoc = V3 (6) 3.5 0
thumb1stRowOffset _ _ = zero

invThumbRen,thumbRenamer :: (Int -> Int -> a) -> Int -> Int -> a
invThumbRen f i j = f (-i) (j-2+homeRow)
thumbRenamer f i j = f (-i) (j+2-homeRow)

thumb' kModel = thumbRenamer (thumb (invThumbRen kModel))
-- >>> main

-- | Hand = thumb + fingers
hand :: (Int -> Int -> Part3 xs R) -> Int -> Int -> Option (Part xs V3 R)
hand = map3 (rotate3d (5 * degree) yAxis) . -- global roll
       -- map3 (rotate3d (-10 * degree) xAxis) . -- global pitch
       (thumb' <> fingers)

catOptions :: [Option a] -> [a]
catOptions xs = [x | Yes x <- xs]

gather :: (Prelude.Num t1, Prelude.Num t2, Enum t1, Enum t2) =>
                (t2 -> t1 -> Option a) -> [a]
gather f = catOptions [f j i | i <- [-8..12], j <- [-8..12]]

withBase :: Floating a => Show a => Field a => ScadV v => Group (v a) => Loc v a -> Part xs v a -> Part xs v a
withBase Loc{..} = translate locPoint . rotate locBase

-- >>> main

-- | Height of a frame segment
segmentHeight :: R
segmentHeight = 3

thickness :: ('[Nadir] ∈ xs, '[Zenith] ∈ xs) => Part3 xs R -> R
thickness p = d zenith nadir
  where d r1 r2 = norm (locPoint (r1 p) - locPoint (r2 p))

-- pinky home row location
pinkyHome :: V3 R
pinkyHome = fingerLoc hand 5 0

pinkyFloor :: V3 R
pinkyFloor = pinkyHome - (20 - necessaryDepthBelowPlate) *< zAxis -- the higher, the closer the floor will be to the keyswitches


splitOffset :: R
splitOffset = screwHeadRoom + 0.6 -- 3 layers at 0.2mmm


-- >>> main

floorReference :: R
floorReference = getZ pinkyFloor

-- | Extend a cube bottom to the floor, thus creating a pillar.
mkPillar :: R -> Part ModelPoints V3 R -> Part3 '[] R
mkPillar ref p = prismoid (zip pillarBottom cubeBottom)
  where cubeBottom = [locPoint ((nadir |<- l) p) | l <- [northWest, southWest, southEast, northEast]]
        pillarBottom = map (zWrite ref) cubeBottom

mapO :: (Int -> Int -> a -> b) -> (Int -> Int -> Option a) -> (Int -> Int -> Option b)
mapO g f i j = g i j <$> f i j

atScrew :: Part3 xs R -> Int -> Int -> Part3 ModelPoints R -> Part3 '[] R
atScrew sh i j p =
  translate (zWrite floorReference (locPoint ((nadir |<- cornerRelloc (i+1) (j+1)) p))) $
  translate ((-3) *< z0 ((V2 (fromIntegral (i `mod` 2)) (fromIntegral (j`mod` 2))) - pure 0.5)) $
  forget $ sh


screwLength :: R
screwLength = 11
-- >>> main

screwHeadLength :: R
screwHeadLength = 3

screwHeadRoom :: R
screwHeadRoom = screwHeadLength + 0.5

screwHeadDiameter :: R
screwHeadDiameter = 6

-- where is this corner relative to the key?
cornerRelloc :: HasCardinals xs => Int -> Int -> RelLoc xs v R
cornerRelloc i' j' = if isNorth
                     then if isWest  then northWest else northEast
                     else if isWest  then southWest else southEast
      where isWest = i' `mod` 2 == 0
            isNorth = j' `mod` 2 == 1

type HasCardinals xs = ('["northWest"] ∈ xs, '["northEast"] ∈ xs, '["southWest"] ∈ xs,'["southEast"] ∈ xs)


-- | double the grid size and create a shape for each corner.
corners :: HasCardinals xs
        => Part2 xs R -> (Int -> Int -> Option (Part3 ModelPoints R))
        -> Int -> Int -> Option (Part3 ('["bottom"] : '["top"] : xs) R)
corners shape f i' j' = corner <$> f (i' `div` 2) (j' `div` 2)
  where relloc :: forall v xs. HasCardinals xs => RelLoc xs v R
        relloc = cornerRelloc i' j'
        loc x = Loc {locPoint = locPoint (relloc x)
                    ,locBase = locBase (zenith x)} -- direction is the same as original
        corner x = withBase (loc x) (extrude (thickness x) (center relloc shape))

-- >>> main

-- | Filter out the parts of the webbing that correspond to the shapes
-- generating corners.
webbing :: (Int -> Int -> Option (Part xs v R)) -> Int -> Int -> Option (Part '[] v R)
webbing f = filterLocs (\_ i j -> not (i `mod` 2 == 0 && j `mod` 2 == 0)) (webbing' f)

webbing' :: (Int -> Int -> Option (Part xs v R)) -> Int -> Int -> Option (Part '[] v R)
webbing' f i j
  | all isNone ps = None
  | otherwise = Yes (hulls (catOptions ps)) -- hull them
  where ps = [f (i+k) (j+l) | k <- [0,1], l <- [0,1]] -- 4 shapes around i,j

frameoid :: R -> (Part3 ModelPoints R -> Part xs v R) -> (Int -> Int -> Part3 ModelPoints R) -> Part '[] v R
frameoid d f g = ug (webbing' (map3 f (corners (rectangle $ pure d) (hand g))))

frame2 :: Part '[] V3 R
frame2 = union (ug $ webbing $ corners (rectangle $ pure segmentHeight) $ hand mountModel)
               (ug $ filterLocs (\_ -> keycapLonger) $ hand $ mountModel)

frameFinal :: Part '[] V3 R
frameFinal = difference (ug (hand mountNegative))
                        frame2


-- mcuLocat :: Part xs V3 R -> Part xs V3 R
-- mcuLocat =
--   translate (zMod (const 0) topLeftFinger + V3 (-34) (28) floorReference) .
--   rotate3d (pi/2) yAxis .
--   mirror zAxis

-- >>> main

-- splitTransform :: R -> Part xs V3 R -> Part xs V3 R
-- splitTransform delta = translate (pinkyFloor + (splitOffset + delta) *< zAxis)


isBorder :: (Int -> Int -> Option a) -> Int -> Int -> Bool
isBorder f i j  = any isNone [f (i+k) (j+l) | k <- [-1,1], l <- [-1,1]]

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

isGlobalCorner :: (Int -> Int -> Option a) -> Int -> Int -> Bool
isGlobalCorner _ 5 5 = True
isGlobalCorner _ i j | (i,j) `elem` excludedCorners = False
isGlobalCorner f i j =
  ((i == 0) && (j == 1)) ||
  (length (filter isYes [f (i+k) (j+l) | k <- [-1,1], l <- [-1,1]]) < 4 -- less than 4 neighbours
    &&
    (count isYes [f (i+k) (j) | k <- [-1,1]] `mod` 2) == (count isYes [f (i) (j+k) | k <- [-1,1]] `mod` 2))
    -- not the same number of neighbours horiz/vert

filterLocs  :: ((Int -> Int -> Option a) -> Int -> Int -> Bool) -> (Int -> Int -> Option a) -> Int -> Int -> Option a
filterLocs p f i j = if p f i j then f i j else None


-- Fit a 8mm drill hole.
wristRestHolderAttachDiameter :: R
wristRestHolderAttachDiameter = 7.7

wristRestHolderAttach :: Part xs V3 R -> Part '[] V3 R
wristRestHolderAttach =
  forget . 
  difference (center nadir $ extrude (screwHeadLength + reflen)$ scale 6 $ circle) .
  difference (center nadir $ extrude (screwHeadLength + reflen+4)$ scale 3.5 $ circle) .
  union (center nadir $ extrude (screwHeadLength + reflen+3.5) $ scale wristRestHolderAttachDiameter circle) -- fits in a 8mm hole
  where reflen = 4

wristRestRef :: V3 R
wristRestRef = zWrite floorReference (fingerLoc hand 3 (-2)) + V3 (keycapWidth/2 + 2) (- 20) 0

-- >>> wristRestRef
-- Euclid {fromEuclid = VNext (VNext (VNext VZero 59.60670574595117) (-79.5402684093055)) (-28.50870042659817)}

-- (V3 60 (-84) floorReference)
wristRestHolder :: Part '[] V3 R
wristRestHolder =
  forget $
  translate wristRestRef $
  translating (V3 (-d) (-e) 0) wristRestHolderAttach $
  translating (V3 d (-e) 0) wristRestHolderAttach $
  center nadir $ extrude 5 $
  difference (rectangle (sz - (1 + sqrt 2 / 2) *< pure (wristRestHolderAttachDiameter))) $
  rectangleWithRoundedCorners (wristRestHolderAttachDiameter / 2) $ V2 w h
  where w = 45
        d = w/2 - wristRestHolderAttachDiameter / 2
        e = 12
        h = 2*e+wristRestHolderAttachDiameter
        sz = V2 w h

-- >>> main

floorAt :: R -> Part '[] V3 R
floorAt z = forget (translate (pinkyFloor + V3 0 0 z) $ center zenith $ extrude 100 $ scale 400 square)

pruneAtFloor :: R -> Part xs V3 R -> Part (xs ++ '[]) V3 R
pruneAtFloor z = difference (floorAt z)


-- >>> main

enclosure2 :: Part '[] V3 R
enclosure2 =
  forget $

  pruneAtFloor 0 $ -- remove anything below/above floor level

  -- hole for battery cable
  -- difference (translate (10 *< zAxis + wristRestRef) $ yOrientation $ extrude 50 $ scale 3 $ circle) $

  
  -- access hatches
  difference (translate (V3 0 0 (floorReference-1)) $ center nadir $ extrude 20 $
               union (translate (V2 (-41) (16) + dropZ (fingerLoc hand 3 0)) $ rectangleWithRoundedCorners 10 (V2 30 60)) $
               (translate (V2 15 0 + dropZ (fingerLoc hand 3 0)) $ rectangleWithRoundedCorners 10 (V2 68 60))) $

  difference (boardRel $ union boardNegativeSpace usbcConnectorNegativeSpace) $
  union (boardRel $ boardSupport) $

  -- remove the interior negative space
  difference (pruneAtFloor 1 $ -- so the bottom plate remains
              union frameNegative interior) $
  
  difference frameSupport $
  
  union wristRestHolder $
  union walls $ -- walls + interior
  frameoid 7 id seats -- main support for the frame
  where
    frameSupport = color (V3 0 1 1) $ frameoid segmentHeight id seatHole -- support for the frame
    frameNegative = color (V3 0 0 1) $ frameoid segmentHeight id thickApprox -- negative space where the frame will fit
    thickApprox i j = translate (V3 0 0 zOfs) $ base (frameThickness+tol+zOfs) (mountSize i j + pure (2*tol))
    seats i j = translate (V3 0 0 (-2)) $ base (frameThickness+4) (keycapSize i j + pure 6)
    walls = frameoid 7 (mkPillar floorReference) seats
    interior = color (V3 1 0 1) $ 
      frameoid 0.1 (mkPillar (floorReference+1)) (\i j -> translate (V3 0 0 0.5) $ seatHole i j)
    seatHole i j = translate (V3 0 0 extra) $ base (frameThickness+4+extra) (keycapSize i j - pure 1)
      where extra = 0
    tol = 0.4
    zOfs = 20

debugCorners :: [(Int, Int)]
debugCorners = [(i,j) | i <- [-8..12], j <- [-8..12], isGlobalCorner cs i j, isYes (cs i j)]
  where seats i j = base (frameThickness) (keycapSize i j)
        cs = corners (rectangle $ pure 0.1) (hand $ seats)

excludedCorners :: [(Int, Int)]
excludedCorners = [(0,-1),(4,-4), (5,-4), (5,-6), (7,-2)]

-- >>> debugCorners
-- [(-4,-4),(-4,-1),(0,-1),(0,1),(0,5),(4,-6),(4,-4),(5,-6),(5,-4),(7,-4),(7,-2),(11,-2),(11,5)]


ug :: (Int -> Int -> Option (Part xs v a)) -> Part '[] v a
ug = unions . gather

instance Module Int R where
  x *^ y = fromIntegral x * y

keysPreview :: Part '[] V3 R
keysPreview = unions $ concat $ [gather (hand keycapModel), gather (hand (\_ _ -> switchModel))] 

keysPreviewPressed :: Part '[] V3 R
keysPreviewPressed = unions $ concat $ [gather (hand (\i j -> translate (V3 0 0 (-stemHeight)) $ keycapModel i j)), gather (hand (\_ _ -> switchModel))]

main :: IO ()
main = do
  writeFile "base.scad" (rndr $ difference (mountNegative 0 0) $ mountModel 0 0 )
  writeFile "box.scad" (rndr $ enclosure2)
  -- writeFile "box-low-l.scad" (rndr $ mirror xAxis $ enclosure2 Bottom)
  -- writeFile "box-high-l.scad" (rndr $ mirror xAxis $ enclosure2 Top)
  -- writeFile "box-high.scad" (rndr $ enclosure2 Top)
  writeFile "f.scad" (rndr $ frameFinal)
  writeFile "f-preview.scad" (rndr $ frame2)
  writeFile "k.scad" (rndr $ keysPreview)
  writeFile "k-pressed.scad" (rndr $ keysPreviewPressed)
  writeFile "integration-test.scad"
    (rndr $ unions
      [
        keysPreview,
        color' 0.7 (V3 0.5 0.5 0.8) $ meshImport "f.stl",
        color (V3 1 0.0 0.0) $ meshImport "box.stl",
        boardRel $ boardAndNin
        -- boardRel $ color (V3 0.7 0.0 0.0) $  boardSupport
        -- , translate (floorProj zero) $ forget $ center zenith $ extrude 10 $ scale 200 $ square -- table
      ])

-- >>> main

batteryRel :: Part xs V3 R -> Part xs (V3) R
batteryRel =  translate (batteryPos) . rotate3d (10 * degree) yAxis
  where batteryPos = lopLeftFloor + V3 (-1) 0 0

boardRel :: Part xs V3 R -> Part xs V3 R
boardRel = translate (floorProj (fingerLoc hand (-1) (-1)) + V3 10 5 boardUndersideClearance) .  rotate3d (120 * degree) zAxis . translate (V3 1.5 4 0)

lopLeftFloor :: V3 R
lopLeftFloor = floorProj (locPoint (nadir pp))
  where Yes pp = corners square (hand mountModel)
                 (0 * 2) (2 * 2 + 1)
                 -- 0,2 is the coordinate of the top-left key. Corners are *2.


floorProj :: V3 R -> V3 R
floorProj = zWrite (getZ pinkyFloor) 

-- >>> main
