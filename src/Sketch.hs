{-# HLINT ignore "Use first" #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sketch
  ( Sketch,
    wrapShape,
    sketchRecord,
    point,
    x,
    y,
    chamfer,
    line,
    onLine,
    from,
    degree,
    between,
    intersectionPoint,
    poly,
    rely,
    relx,
    onYAxis,
    wideLine,
    rectSketch,
    rectCenter,
    sketchExtrude,
    ExtrudeAxis (..),
    expandVector,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer (runWriter, tell)
import Data.Function ((&))
import OpenSCAD (Model2d, Model3d, Vector2d, Vector3d, linearExtrudeDefault, mirror, rotate3d, translate)
import SketchSolver (runSolver')
import SketchTypes
import Prelude hiding (id)
import qualified Prelude

sketchRecord :: (Models m) => SketchM m -> Res m
sketchRecord m =
  m
    & fmap toList
    & runState 0
    & fmap fst
    & runWriter
    & run
    & ( \((sks, proxy), cs) ->
          runSolver' (sks, cs)
            & either (error . show) (fromList . (,proxy))
      )

--- POINT

point :: SketchM Point
point = Point <$> genId <*> genId <*> pure 0

x :: Double -> SketchM Point -> SketchM Point
x val m = do
  (Point x_ y_ cham) <- m
  putExact x_ val
  pure $ Point x_ y_ cham

y :: Double -> SketchM Point -> SketchM Point
y val m = do
  (Point x_ y_ cham) <- m
  putExact y_ val
  pure $ Point x_ y_ cham

chamfer :: Double -> SketchM Point -> SketchM Point
chamfer val m = do
  (Point x_ y_ _) <- m
  pure $ Point x_ y_ val

relx :: Point -> Double -> SketchM Point -> SketchM Point
relx fromPoint distance m = do
  newPoint <- m
  putPlus newPoint.x fromPoint.x distance
  pure newPoint

rely :: Point -> Double -> SketchM Point -> SketchM Point
rely fromPoint distance m = do
  newPoint <- m
  putPlus newPoint.y fromPoint.y distance
  pure newPoint

--- LINE

line :: SketchM Line
line = do
  x_ <- genId
  y_ <- genId
  Line x_ y_ <$> genId

from :: Point -> SketchM Line -> SketchM Line
from (Point x_ y_ _) m = do
  l <- m
  putEq l.x x_
  putEq l.y y_
  pure l

degree :: Angle -> SketchM Line -> SketchM Line
degree val m = do
  l <- m
  putExact l.angle (floor val & (\(v :: Int) -> v `mod` 360) & fromIntegral)
  pure l

onLine :: Line -> Point -> SketchM Point
onLine l p = do
  tell [OnLine p l]
  pure p

between :: Point -> Point -> SketchM Line -> SketchM Line
between p1 p2 m = do
  l <- m
  _ <- onLine l p1
  _ <- onLine l p2
  pure l

wideLine :: Double -> Point -> Point -> SketchM Polygon
wideLine width f t = do
  a <- point
  b <- point
  c <- point
  d <- point
  tell [WideLine width (f, t) (a, b, c, d)]
  poly [a, b, c, d]

-- RECT

rectSketch :: SketchM Point -> (Point -> SketchM Point) -> SketchM (Point, Point, Point, Point)
rectSketch am cm = do
  a <- am
  c <- cm a
  b <- point & relx c 0 & rely a 0
  d <- point & relx a 0 & rely c 0
  pure (a, b, c, d)

--- POLYGON
poly :: [Point] -> SketchM Polygon
poly = pure . Polygon

rectCenter :: Point -> Point -> Point -> Point -> SketchM Point
rectCenter a b c d = do
  v1 <- line & between a c
  v2 <- line & between b d
  intersectionPoint v1 v2

--- INTERSECTION POINT

intersectionPoint :: Line -> Line -> SketchM Point
intersectionPoint l1 l2 = do
  p <- point
  tell [Intersection l1 l2 p]
  _ <- onLine l1 p
  _ <- onLine l2 p
  pure p

-- helpers
putExact :: Id -> Double -> SketchM ()
putExact id v = tell [Exact id v]

putEq :: Id -> Id -> SketchM ()
putEq id1 id2 = tell [Eq id1 id2]

putPlus :: Id -> Id -> Double -> SketchM ()
putPlus idl idr distance = do
  tell [Plus idl idr distance]

genId :: SketchM Id
genId = do
  i <- get
  put (i + 1)
  pure $ Id i

-- utils for user
onYAxis :: Model3d -> Model3d
onYAxis m = m & rotate3d (90, 0, 0) & mirror (0, 1, 0)

onXAxis :: Model3d -> Model3d
onXAxis m = m & rotate3d (0, 0, 90) & rotate3d (0, 90, 0)

data ExtrudeAxis = OnXAxis | OnYAxis | OnZAxis
  deriving (Show, Eq)

sketchExtrude :: Double -> Double -> ExtrudeAxis -> Model2d -> Model3d
sketchExtrude bottom top axis model =
  model
    & OpenSCAD.linearExtrudeDefault (top - bottom)
    & OpenSCAD.translate (0, 0, bottom)
    & case axis of
      OnXAxis -> onXAxis
      OnYAxis -> onYAxis
      OnZAxis -> Prelude.id

expandVector :: ExtrudeAxis -> Vector2d -> Vector3d
expandVector OnXAxis (y_, z_) = (0, y_, z_)
expandVector OnYAxis (x_, z_) = (x_, 0, z_)
expandVector OnZAxis (x_, y_) = (x_, y_, 0)
