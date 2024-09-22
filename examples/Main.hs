{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Data.Function ((&))
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

data Z = Z {base :: Polygon, centerPoint :: Point}

mkSketchRes ''Z

data Y = Y {ySide :: Polygon}

mkSketchRes ''Y

data X = X {xSide :: Polygon}

mkSketchRes ''X

main :: IO ()
main =
  do
    let resz =
          sketch do
            -- can't use rectSketch for contradiction on using rectCenter
            a <- point & x 0 & y 0
            b <- point & x 4 & y 0
            c <- point & x 4 & y 4
            d <- point & x 0 & y 4
            base <- poly [a, b, c, d]
            centerPoint <- rectCenter a b c d
            pure $ Z {..}
    let resy = sketch do
          (a, b, c, d) <- rectSketch (point & x 0 & y 0) (\a_ -> point & relx a_ 4 & rely a_ 4)
          ySide <- poly [a, b, c, d]
          pure $ Y {..}
    let resx = sketch do
          a <- point & x 0 & y 0
          b <- point & x 4 & y 0
          c <- point & relx b (-0.5) & y 4
          d <- point & relx a 0.5 & y 4
          xSide <- poly [a, b, c, d]
          pure $ X {..}
    let obj =
          sketchExtrude 0 10 OnZAxis resz.base
            & with intersection (sketchExtrude 0 10 OnYAxis resy.ySide)
            & with intersection (sketchExtrude 0 10 OnXAxis resx.xSide)
            & diff (cylinder 10 1 def & translate (expandVector OnZAxis resz.centerPoint))
    render (pure obj) & writeFile "main.scad"
