-- JPP Haskell
-- Pierwsze zadanie zaliczeniowe
-- Aliaksei Suvorau as374118

module Lib where

import           Data.Ratio
import           Mon

type R = Rational
type R2 = (R, R)
type IntLine = ((Int, Int), (Int, Int))
type IntRendering = [IntLine]
type Line = (Point, Point)

data Vec = Vct R R deriving (Eq, Show)
data Point  = Pt R R deriving (Eq, Show)
data Picture = Pic [Line]
data SimpleTransform = Translation Vec | Rotation R deriving (Eq, Show)
data Transform = Tr [SimpleTransform] deriving (Eq, Show)

instance Mon Vec where
  m1 = Vct 0 0
  (><) (Vct x1 y1) (Vct x2 y2) = Vct (x1 + x2) (y1 + y2)

instance Mon Transform where
  m1 = Tr []
  (><) = composeTransforms

---------- RYSOWANIE ----------

ptZero :: Point
ptZero = Pt 0 0

point :: R2 -> Point
point (x, y) = Pt x y

vec :: R2 -> Vec
vec (x, y) = Vct x y

line :: (R, R) -> (R, R) -> Picture
line p1 p2 = Pic [(point p1, point p2)]

rectangle :: R -> R -> Picture
rectangle width height = Pic
    [ ( ptZero
      , point (width, 0)
      ) -- do gory
    , ( point (width, 0)
      , point (width, height)
      ) -- w prawo
    , ( point (width, height)
      , point (0, height)
      ) -- do dolu
    , (point (0, height), ptZero)
    ] -- w lewo

(&) :: Picture -> Picture -> Picture
(&) (Pic l1) (Pic l2) = Pic (l1 ++ l2)

---------- RENDEROWANIE ----------

renderScaled :: Int -> Picture -> IntRendering
renderScaled scale (Pic lines) = map (scaleLineAndRender scale) lines
  where
    scaleLineAndRender scale line = renderLine (scaleLine scale line)
    scaleLine scale (p1, p2) = (scalePoint scale p1, scalePoint scale p2)
    scalePoint scale (Pt x y) = Pt ((toRational scale) * x) ((toRational scale) * y)

renderLine :: Line -> IntLine
renderLine (p1, p2) = (renderPoint p1, renderPoint p2)

renderPoint :: Point -> (Int, Int)
renderPoint (Pt x y) = (round x, round y)

---------- TRANSFORMACJE ----------

translate :: Vec -> Transform
translate v = Tr [Translation v]

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
rotate :: R -> Transform
rotate r = Tr [Rotation r]

fullCircle :: R
fullCircle = 360

halfCircle :: R
halfCircle = fullCircle / 2

fullCircleInt :: Int
fullCircleInt = round fullCircle

-- Funkcje pomocnicze dla pojedynzcych transformacji

normaliseDegrees :: R -> R
normaliseDegrees r
    | r > 0     = r - toRational ((div (round r) fullCircleInt) * fullCircleInt)
    | otherwise = r + toRational (((div (-(round r)) fullCircleInt) + 1) * fullCircleInt)

sinus :: R -> R
sinus x = if x' < halfCircle then bhaskara x' else -(bhaskara (x' - halfCircle))
  where
    bhaskara x = (4 * x * (halfCircle - x)) / (40500 - x * (halfCircle - x))
    x' = normaliseDegrees x -- zawęzamy do zakresu 0 - 360 stopni

cosinus :: R -> R
cosinus x = sinus (x + halfCircle / 2)

rotateCoords :: R -> R2 -> R2
rotateCoords r (x, y) = (x', y')
  where
    x' = x * (cosinus r) - y * (sinus r)
    y' = y * (cosinus r) + x * (sinus r)

trpointSimple :: SimpleTransform -> Point -> Point
trpointSimple (Translation (Vct vx vy)) (Pt ptx pty) = Pt (vx + ptx) (vy + pty)
trpointSimple (Rotation r) (Pt x y) = Pt x' y' where (x', y') = rotateCoords r (x, y)

trvecSimple :: SimpleTransform -> Vec -> Vec
trvecSimple (Translation _) vector    = vector
trvecSimple (Rotation    r) (Vct x y) = Vct x' y' where (x', y') = rotateCoords r (x, y)

trLineSimple :: SimpleTransform -> Line -> Line
trLineSimple tr (p1, p2) = (trpointSimple tr p1, trpointSimple tr p2)

transformSimple :: SimpleTransform -> Picture -> Picture
transformSimple tr (Pic lines) = Pic (map (trLineSimple tr) lines)

optimiseSimpleTransformList :: [SimpleTransform] -> [SimpleTransform]
optimiseSimpleTransformList (tr1 : (tr2 : tail)) = case (tr1, tr2) of
    (Rotation r1, Rotation r2) -> (Rotation (r1 + r2)) : (optimiseSimpleTransformList tail)
    _                          -> (tr1 : optimiseSimpleTransformList (tr2 : tail))
optimiseSimpleTransformList l = l

-- Funkcje dla listy transformacji

applyListOfSimpleTransforms :: Transform -> (SimpleTransform -> a -> a) -> a -> a
applyListOfSimpleTransforms (Tr simpleTransformList) simpleTrFun el =
    foldl (\el -> \tr -> simpleTrFun tr el) el simpleTransformList

trpoint :: Transform -> Point -> Point
trpoint tr = applyListOfSimpleTransforms tr trpointSimple

trvec :: Transform -> Vec -> Vec
trvec tr = applyListOfSimpleTransforms tr trvecSimple

transform :: Transform -> Picture -> Picture
transform tr = applyListOfSimpleTransforms tr transformSimple

composeTransforms :: Transform -> Transform -> Transform
composeTransforms (Tr tr1) (Tr tr2) = Tr $ optimiseSimpleTransformList (tr1 ++ tr2)
