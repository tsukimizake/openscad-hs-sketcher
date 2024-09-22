module SketchTypes where

import Control.Monad.Freer
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy
import OpenSCAD (Model2d, Vector2d)
import Prelude hiding (pi)

newtype Id = Id Int
  deriving (Eq, Ord)

newtype UnionFind = UnionFind {parent :: Map Id Id} deriving (Show, Eq)

type SketchM = Eff '[State Int, Writer [Constraint]]

type Angle = Double

instance Show Id where
  show (Id i) = "id" ++ show i

data Sketch
  = P Point
  | LineFunc Line
  | Poly Polygon
  deriving (Show, Eq, Ord)

data Point = Point {x :: Id, y :: Id, chamfer :: Double}
  deriving (Show, Eq, Ord)

instance Shape Point where
  wrapShape = P

data Line = Line {x :: Id, y :: Id, angle :: Id}
  deriving (Show, Eq, Ord)

instance Shape Line where
  wrapShape = LineFunc

newtype Polygon = Polygon [Point]
  deriving (Show, Eq, Ord)

instance Shape Polygon where
  wrapShape = Poly

data SketchError
  = Contradiction String
  | Unresolved String
  deriving (Show, Eq, Ord)

data Constraint
  = Exact Id Double
  | Eq Id Id
  | OnLine Point Line
  | Plus Id Id Double --  left = right + distance
  | Intersection Line Line Point
  | WideLine Double (Point, Point) (Point, Point, Point, Point)
  deriving (Show, Eq, Ord)

class Shape a where
  wrapShape :: a -> Sketch

type Intersections = [(Line, Line, Point)]

type OnLines = [(Point, Line)]

type Sketches = [Sketch]

type Exacts = [(Id, Double)]

type Eqs = [(Id, Id)]

type Pluses = [(Id, Id, Double)]

type WideLines = [(Double, (Point, Point), (Point, Point, Point, Point))]

type SolverM = Eff '[State (UnionFind, Eqs, Exacts, Pluses), Reader (OnLines, Sketches, Intersections, WideLines), Error SketchError]

data Result
  = ModelRes Model2d
  | PointRes Vector2d
  deriving (Show)

unwrapModelRes :: Result -> Model2d
unwrapModelRes = \case
  ModelRes m -> m
  _ -> error "unwrapModelRes"

unwrapPointRes :: Result -> Vector2d
unwrapPointRes = \case
  PointRes m -> m
  _ -> error "unwrapPointRes"

class Models a where
  type Res a :: Type
  toList :: a -> ([Sketch], Proxy a)
  fromList :: ([Result], Proxy a) -> Res a
