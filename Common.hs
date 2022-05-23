{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Common where
import HCad
import Algebra.Linear ()
import Algebra.Classes
import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.), Floating(..))
import Data.List ( intercalate)
import Data.Foldable

data Option a = Yes {fromYes :: a} | None deriving Functor

isYes :: Option a -> Bool
isYes = not . isNone
isNone :: Option a -> Bool
isNone None = True
isNone _ = False
instance Semigroup (Option a) where
  Yes x <> _ = Yes x
  None <> x = x
instance Monoid (Option a) where
  mempty = None


type R = Double

rndr :: Foldable v => Functor v => Part xs v R -> String
rndr = render myOpts

myOpts :: Options
myOpts = defaultOptions {optFn = 20}

inch :: Double
inch = 25.4

xOrientation,yOrientation :: Part xs V3 R -> Part xs V3 R
xOrientation = rotate (rotation3d (pi/2) yAxis)
yOrientation = rotate (rotation3d (pi/2) xAxis)


rotate3d :: (Show s, Transcendental s, Ring s) =>
                  s -> V3 s -> Part xs V3 s -> Part xs V3 s
rotate3d angle axis = rotate (rotation3d angle axis)


degree :: R
degree = pi / 180

-- >>> 15*degree
-- 0.17453292519943295

xy :: Euclid V3' a -> Euclid V2' a
xy (V3 x y _) = V2 x y

data Direction = NW | NE | SE | SW

dir :: Direction -> V2 R
dir NW = V2 (-1) (-1)
dir SW = V2 (-1) (1)
dir SE = V2 (1) (1)
dir NE = V2 (1) (-1)



truncatedPyramid :: R -> V2 R -> V2 R -> Part '[] V3 R
truncatedPyramid height lo hi = prismoid [(zSet (-height) ((0.5::Double) *^ (dir d⊙ lo))
                                          ,z0 ((0.5::Double) *^ (dir d ⊙ hi))) | d <- [NW, NE, SE, SW] ]



relativeTo :: (ScadV v, Ring s, Show s, Group (v s)) =>
                  v s
                  -> (Part xs1 v s -> Part xs2 v s) -> Part xs1 v s -> Part xs2 v s
relativeTo x f = translate x . f . translate (negate x)



prismoid :: Ord a => [(V3 a, V3 a)] -> Part '[] V3 a
prismoid [] = error "prismoid: needs at least three elements"
prismoid ps@(x:xs) = polyhedron (top:reverse bottom:[[a,b,c,d] | ((a,b),(d,c)) <- zip (x:xs) (xs++[x])])
   where (top,bottom) = unzip ps


z0 :: Additive a => Euclid v a -> Euclid (VNext v) a
z0 = zSet zero 

zSet :: a -> Euclid v a -> Euclid (VNext v) a
zSet x (Euclid v) = Euclid (VNext v x) 

zWrite :: a -> Euclid (VNext v) a -> Euclid (VNext v) a
zWrite = zMod . const

zMod :: (a -> a) -> Euclid (VNext v) a -> Euclid (VNext v) a
zMod f (Euclid (VNext v x)) = Euclid (VNext v (f x))

getXin2 :: V2 a -> a
getXin2 (V2 x _) = x

getZ :: Euclid (VNext v) a -> a
getZ (Euclid (VNext _ x)) = x

dropZ :: Euclid (VNext f) a -> Euclid f a
dropZ (Euclid (VNext x _)) = Euclid x


hullCycle :: [Part xs v a] -> Part '[] v a
hullCycle (p:ps) = unions $ zipWith hull  (p:ps) (ps++[p])

walkBorder :: (Int -> Int -> Option a) -> [a]
walkBorder f = map (fromYes . check)  points
  where start = findStart zero
        findStart p | isNone (check (p+d0)) = p
                    | otherwise = findStart (p+d0)
        points = go (leftOf d0) start
        go d p = case find (isYes . check . (+p))  ([rightOf d, d, leftOf d]) of
                   Just d' -> p : if p+d' == start then [] else go d' (p+d')
        d0 = V2 0 1
        check (V2 i j) = f i j
        leftOf (V2 x y) = V2 (-y) (x)
        rightOf (V2 x y) = V2 y (-x)


angles' :: [V2 R] -> [R]
angles' (p:q:r:ps) = cosAngle : angles' (q:r:ps)
  where cosAngle = (((p - q) · (q - r)) / norm (p-q)) / norm (q-r)
angles' _ = []

angles :: [V2 R] -> [R]
angles xs = angles' $ last xs : xs ++ [head xs]

dist :: (InnerProdSpace v, Group (v s), Field s, Roots s) => (v s, v s) -> s
dist (x,y) = norm (x-y)

rot :: [a] -> [a]
rot (x:xs) = xs ++ [x]
invRot :: [a] -> [a]
invRot xs = last xs : init xs

distances ps = zip (invRot ds) ds
  where ds = (map dist (zip ps (rot ps)))

isSharpTurn a dl dr = a < -0.5 && (dl > 5 || dr > 5)


killSharpTurns :: [Loc V3 R] -> [Loc V3 R]
killSharpTurns locs = [p | (p,a,(dl,dr)) <- (zip3 locs as ds), not (isSharpTurn a dl dr)]
  where as = angles ps
        ps = map (dropZ . locPoint) locs
        ds = distances ps


showV :: Show a => Foldable f => Euclid f a -> [Char]
showV (Euclid v) = "(" ++ intercalate "," (map show $ toList v)  ++ ")"


projectOnPlaneAlongDirecton
  :: forall scalar. (Module scalar scalar, Field scalar) =>
     Loc V3 scalar -> V3 scalar -> V3 scalar -> V3 scalar
projectOnPlaneAlongDirecton plane@Loc {locPoint = planeOrigin} d p0 = p1
 where θ :: scalar
       θ = (planeOrigin - p0) · planeNormal / (d · planeNormal)
       p1 :: V3 scalar
       p1 = θ *^ d + p0
       planeNormal = locNormal plane
       -- equation of plane:  (x-planeOrigin) · planeNormal = 0
       -- parametric equation of line :  x = p0 + θ d
       -- equation for θ: (θ d + p0 - planeOrigin) · planeNormal = 0
       -- distribute: θ (d · planeNormal) + (p0 - planeOrigin) · planeNormal = 0
       -- solving for θ gives the above equation


locating :: (ScadV v, Show s, Field s, Group (v s)) =>
                  Loc v s
                  -> (Part xs1 v s -> Part xs2 v s) -> Part xs1 v s -> Part xs2 v s
locating Loc{..} f = translating locPoint (rotating locBase f)
