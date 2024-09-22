{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module SketchSolver (runSolver, runSolver') where

import Control.Applicative (liftA3)
import Control.Monad (forM)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State
import Data.Either
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind (Type)
import qualified Data.List as List
import Data.Maybe
import OpenSCAD (Model2d, Vector2d, polygon)
import SketchTypes
import UnionFind (emptyUF, find, union)
import Prelude hiding (atan2, cos, id, sin, tan)
import qualified Prelude

data SolverState = SolverState
  { uf :: UnionFind,
    onLines :: OnLines,
    exacts :: Exacts,
    pluses :: Pluses,
    eqs :: Eqs,
    sketches :: Sketches,
    intersections :: Intersections,
    wideLines :: WideLines
  }

readStat :: SolverM SolverState
readStat = do
  (uf, eqs, exacts, pluses) <- get
  (onLines, sk, intersections, wideLines) <- ask
  pure $ SolverState uf onLines exacts pluses eqs sk intersections wideLines

runSolver' :: ([Sketch], [Constraint]) -> Either SketchError [Result]
runSolver' (models, cs) =
  let onLines = mapMaybe (\case OnLine p l -> Just (p, l); _ -> Nothing) cs
      exacts = mapMaybe (\case Exact id v -> Just (id, v); _ -> Nothing) cs
      eqs = mapMaybe (\case Eq l r -> Just (l, r); _ -> Nothing) cs
      intersections = mapMaybe (\case Intersection l r p -> Just (l, r, p); _ -> Nothing) cs
      pluses = mapMaybe (\case Plus l r d -> Just (l, r, d); _ -> Nothing) cs
      wideLines = mapMaybe (\case WideLine w l r -> Just (w, l, r); _ -> Nothing) cs
   in ( repeatUntilFixpoint (solveIntersections >> solveOnLines >> solvePluses >> solveWideLines >> solveUf)
          >> validateAllJust
          >> generateModel
      )
        & runState (emptyUF, eqs, exacts, pluses)
        & runReader (onLines, models, intersections, wideLines)
        & runError
        & run
        & fmap fst

runSolver :: (([Sketch], [Point]), [Constraint]) -> Either SketchError ([Model2d], [Vector2d])
runSolver ((sketches, points), cs) =
  let onLines = mapMaybe (\case OnLine p l -> Just (p, l); _ -> Nothing) cs
      exacts = mapMaybe (\case Exact id v -> Just (id, v); _ -> Nothing) cs
      eqs = mapMaybe (\case Eq l r -> Just (l, r); _ -> Nothing) cs
      intersections = mapMaybe (\case Intersection l r p -> Just (l, r, p); _ -> Nothing) cs
      pluses = mapMaybe (\case Plus l r d -> Just (l, r, d); _ -> Nothing) cs
      wideLines = mapMaybe (\case WideLine w l r -> Just (w, l, r); _ -> Nothing) cs
   in ( repeatUntilFixpoint (solveIntersections >> solveOnLines >> solvePluses >> solveWideLines >> solveUf)
          >> validateAllJust
          >> generateModel
          <&> partitionEithers . List.map \case
            ModelRes m -> Left m
            PointRes (px, py) -> Right (px, py)
      )
        & runState (emptyUF, eqs, exacts, pluses)
        & runReader (onLines, sketches ++ List.map P points, intersections, wideLines)
        & runError
        & run
        & fmap fst

repeatUntilFixpoint :: SolverM a -> SolverM a
repeatUntilFixpoint m = do
  -- repeat until state is not changed
  beforeStat <- readStat
  res <- m
  afterStat <- readStat
  if beforeStat.uf == afterStat.uf && beforeStat.exacts == afterStat.exacts
    then
      pure res
    else repeatUntilFixpoint m

--------------
-- Finalize
--------------

generateModel :: SolverM [Result]
generateModel = do
  SolverState {sketches} <- readStat
  mapM generateModelImpl sketches
  where
    generateModelImpl :: Sketch -> SolverM Result
    generateModelImpl (Poly (Polygon ps)) = do
      rs <- forM (zip3 (drop (length ps - 1) $ cycle ps) ps (drop 1 $ cycle ps)) $
        \(prev, Point x y chamfer, next) -> do
          x' <- getValue x >>= assertJust
          y' <- getValue y >>= assertJust
          if chamfer == 0
            then
              pure [(x', y')]
            else do
              ~(Just prevx, Just prevy) <- getValue prev
              ~(Just nextx, Just nexty) <- getValue next
              let getCosSin (xl, yl) (xr, yr) =
                    let dx = xr - xl
                        dy = yr - yl
                        len = sqrt (dx * dx + dy * dy)
                        c = dx / len
                        s = dy / len
                     in (c, s)
                  (cos1, sin1) = getCosSin (prevx, prevy) (x', y')
                  (cos2, sin2) = getCosSin (x', y') (nextx, nexty)
                  x1 = x' - cos1 * chamfer
                  y1 = y' - sin1 * chamfer
                  x2 = x' + cos2 * chamfer
                  y2 = y' + sin2 * chamfer
              pure [(x1, y1), (x2, y2)]

      pure $ ModelRes $ polygon 3 [concat rs]
    generateModelImpl (P p) = do
      x <- getValue p.x >>= assertJust
      y <- getValue p.y >>= assertJust
      pure $ PointRes (x, y)
    generateModelImpl x = do
      undefined

validateAllJust :: SolverM ()
validateAllJust = do
  SolverState {sketches} <- readStat
  mapM_ validateAllJustImpl sketches
  where
    validateAllJustImpl :: Sketch -> SolverM ()
    validateAllJustImpl sk@(P p) =
      liftA2 (,) (isSolved p.x) (isSolved p.y) >>= \case
        (True, True) -> pure ()
        _ -> throwError (Unresolved $ show sk)
    validateAllJustImpl sk@(LineFunc (Line lx ly angle)) =
      liftA3 (,,) (isSolved lx) (isSolved ly) (isSolved angle) >>= \case
        (True, True, True) -> pure ()
        _ -> throwError (Unresolved $ show sk)
    validateAllJustImpl (Poly (Polygon ps)) = do
      _ <- ps & mapM (validateAllJustImpl . P)
      pure ()

isSolved :: Id -> SolverM Bool
isSolved id = getValue id & fmap isJust

--------------
-- Intersections
--------------
solveIntersections :: SolverM ()
solveIntersections = do
  SolverState {intersections} <- readStat
  mapM_ solveIntersection intersections

solveIntersection :: (Line, Line, Point) -> SolverM ()
solveIntersection (l1, l2, p) =
  liftA3 (,,) (getValue l1) (getValue l2) (getValue p) >>= \case
    ((Just x1, Just y1, Just angle1), (Just x2, Just y2, Just angle2), (Nothing, Nothing)) -> do
      case (angle1, angle2) of
        (0, _) ->
          putEq p.y l1.y
        (90, _) ->
          putEq p.x l1.x
        (180, _) ->
          putEq p.y l1.y
        (270, _) ->
          putEq p.x l1.x
        (_, 0) ->
          putEq p.y l2.y
        (_, 90) ->
          putEq p.x l2.x
        (_, 180) ->
          putEq p.y l2.y
        (_, 270) ->
          putEq p.x l2.x
        _ -> do
          let (x, y) = calcIntersection (x1, y1, angle1) (x2, y2, angle2)
          putExact p.x x
          putExact p.y y
    _ -> pure ()

calcIntersection :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double)
calcIntersection (x1, y1, angle1) (x2, y2, angle2) =
  let a1 = tan angle1
      a2 = tan angle2
      x = (a1 * x1 - a2 * x2 + y2 - y1) / (a1 - a2)
      y = a1 * (x - x1) + y1
   in (x, y)

--------------
-- OnLines
--------------

solveOnLines :: SolverM ()
solveOnLines = do
  SolverState {onLines} <- readStat
  mapM_ solveOnLine onLines

solveOnLine :: (Point, Line) -> SolverM ()
solveOnLine (p, l) = do
  angle_ <- getValue l.angle
  case angle_ of
    Just 0 -> putEq p.y l.y
    Just 90 -> putEq p.x l.x
    Just 180 -> putEq p.y l.y
    Just 270 -> putEq p.x l.x
    _ ->
      liftA2 (,) (getValue p) (getValue l)
        >>= \case
          ((Nothing, Just y), (Just lx, Just _ly, Just angle)) -> do
            let x = lx + (y - _ly) / tan angle
            putExact p.x x
          ((Just x_, Nothing), (Just _lx, Just ly, Just angle)) -> do
            let y = ly + tan angle * (x_ - _lx)
            putExact p.y y
          ((Just x, Just y), (Nothing, Just _ly, Just angle)) -> do
            let lx = x - (y - _ly) / tan angle
            putExact l.x lx
          ((Just x, Just y), (Just _lx, Nothing, Just angle)) -> do
            let ly = y - tan angle * (x - _lx)
            putExact l.y ly
          ((Just x, Just y), (Just lx, Just ly, Nothing)) -> do
            let angle = atan2 (y - ly) (x - lx)
            putExact l.angle angle
          ((Just x, Just y), (Nothing, Nothing, _)) -> do
            putExact l.x x
            putExact l.y y
          _ -> pure ()

--------------
-- UF
--------------

solveUf :: SolverM ()
solveUf = do
  SolverState {eqs} <- readStat
  mapM_ (uncurry unifyIds) eqs

unifyIds :: Id -> Id -> SolverM ()
unifyIds l r =
  liftA2 (,) (getValue l) (getValue r) >>= \case
    (Just _, Nothing) -> do
      updateUf l r
    (Nothing, Just _) -> do
      updateUf r l
    (Just lv, Just rv) -> do
      if lv == rv
        then pure ()
        else
          throwContradiction (l, lv) (r, rv)
    (Nothing, Nothing) -> do
      pure ()

----------
-- Pluses
----------

solvePluses :: SolverM ()
solvePluses = do
  SolverState {pluses} <- readStat
  mapM_ solvePlus pluses

solvePlus :: (Id, Id, Double) -> SolverM ()
solvePlus (l, r, diff) =
  liftA2 (,) (getValue l) (getValue r) >>= \case
    (Just lv, Just rv) -> do
      if lv == rv + diff
        then pure ()
        else do
          throwContradictionWithDiff (l, lv) (r, rv) diff
    (Just lv, Nothing) -> do
      putExact r (diff - lv)
    (Nothing, Just rv) -> do
      putExact l (diff + rv)
    _ -> pure ()

----------
-- WideLines
----------

solveWideLines :: SolverM ()
solveWideLines = do
  SolverState {wideLines} <- readStat
  mapM_ solveWideLine wideLines

solveWideLine :: (Double, (Point, Point), (Point, Point, Point, Point)) -> SolverM ()
solveWideLine (width, (f, t), (a, b, c, d)) =
  (,,,,,) <$> getValue f <*> getValue t <*> getValue a <*> getValue b <*> getValue c <*> getValue d >>= \case
    ((Just fx, Just fy), (Just tx, Just ty), (Nothing, Nothing), (Nothing, Nothing), (Nothing, Nothing), (Nothing, Nothing)) -> do
      let angle = atan2 (ty - fy) (tx - fx)
          ax = fx + cos (angle - 90) * width
          ay = fy + sin (angle - 90) * width
      putExact a.x ax
      putExact a.y ay
      let bx = tx + cos (angle - 90) * width
          by = ty + sin (angle - 90) * width
      putExact b.x bx
      putExact b.y by
      let cx = tx + cos (angle + 90) * width
          cy = ty + sin (angle + 90) * width
      putExact c.x cx
      putExact c.y cy
      let dx = fx + cos (angle + 90) * width
          dy = fy + sin (angle + 90) * width
      putExact d.x dx
      putExact d.y dy
    _ -> pure ()

----------
-- helpers
----------

class HasValue a where
  type Value a :: Type
  getValue :: a -> SolverM (Value a)

instance HasValue Id where
  type Value Id = Maybe Double
  getValue id = do
    SolverState {uf, exacts} <- readStat
    case find id uf of
      parent -> pure $ lookup parent exacts

instance HasValue Point where
  type Value Point = (Maybe Double, Maybe Double)
  getValue p = do
    x <- getValue p.x
    y <- getValue p.y
    pure (x, y)

instance HasValue Line where
  type Value Line = (Maybe Double, Maybe Double, Maybe Double)
  getValue l = do
    x <- getValue l.x
    y <- getValue l.y
    angle <- getValue l.angle
    pure (x, y, angle)

assertJust :: Maybe a -> SolverM a
assertJust = \case
  Just a -> pure a
  _ -> error "value is not resolved"

parentIsExact :: Id -> SolverM Bool
parentIsExact id = do
  SolverState {uf, exacts} <- readStat
  pure $ isExact id uf exacts
  where
    isExact :: Id -> UnionFind -> Exacts -> Bool
    isExact id_ uf exacts =
      case find id_ uf of
        parent -> case lookup parent exacts of
          Just _ -> True
          _ -> False

updateUf :: Id -> Id -> SolverM ()
updateUf l r = do
  stat <- readStat
  put (UnionFind.union l r stat.uf, stat.eqs, stat.exacts, stat.pluses)

putExact :: Id -> Double -> SolverM ()
putExact id v = do
  stat <- readStat
  let exacts = (id, v) : stat.exacts
  put (stat.uf, stat.eqs, exacts, stat.pluses)

putEq :: Id -> Id -> SolverM ()
putEq id1 id2 = do
  stat <- readStat
  let eqs = (id1, id2) : stat.eqs
  put (stat.uf, eqs, stat.exacts, stat.pluses)

cos :: (Floating b) => b -> b
cos degree = degree * (pi / 180) & Prelude.cos

sin :: (Floating b) => b -> b
sin degree = degree * (pi / 180) & Prelude.sin

tan :: (Floating b) => b -> b
tan degree = degree * (pi / 180) & Prelude.tan

atan2 :: Double -> Double -> Double
atan2 y x = Prelude.atan2 y x * 180 / pi

----------
-- error functions
----------

throwContradiction :: (Id, Double) -> (Id, Double) -> SolverM ()
throwContradiction (l, lv) (r, rv) = do
  SolverState {sketches} <- readStat

  throwError
    ( Contradiction $
        "Exact values are not equal: "
          ++ (show l ++ ":" ++ show lv)
          ++ ", "
          ++ (show r ++ ":" ++ show rv)
          ++ "\nin "
          ++ show sketches
    )

throwContradictionWithDiff :: (Id, Double) -> (Id, Double) -> Double -> SolverM ()
throwContradictionWithDiff (l, lv) (r, rv) diff = do
  SolverState {sketches} <- readStat

  throwError
    ( Contradiction $
        "Exact values are not equal: "
          ++ (show l ++ ":" ++ show lv)
          ++ ", "
          ++ (show r ++ ":" ++ show rv)
          ++ (" + diff:" ++ show diff)
          ++ "in "
          ++ show sketches
    )
