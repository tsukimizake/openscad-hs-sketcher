{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Data.Function ((&))
import OpenSCAD
import Sketch
import Test.Hspec

main :: IO ()
main = hspec $ describe "SketchExamples" $ do
  it "rect" do
    let rect = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 4 & y 0
          c <- point & x 4 & y 4
          d <- point & x 0 & y 4
          poly [a, b, c, d]
    rect `shouldBe` (polygon 3 [[(0, 0), (4, 0), (4, 4), (0, 4)]])

  it "pitagoras1" do
    let pitagoras1 = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 4 & y 0
          v1 <- line & from a & degree 30
          v2 <- line & from b & degree 90
          c <- intersectionPoint v1 v2
          poly [a, b, c]
    pitagoras1 `shouldBe` (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 2.3094010767585025)]])

  it "pitagoras2" do
    let pitagoras2 = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 4 -- y is not set, but solved with constraints
          v1 <- line & from a & degree 0
          onLine v1 b
          v2 <- line & from a & degree 30
          v3 <- line & from b & degree 90
          c <- intersectionPoint v2 v3
          poly [a, b, c]
    pitagoras2 `shouldBe` (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 2.3094010767585025)]])
  it "pitagoras3" do
    let pitagoras3 = sketchPoly do
          a <- point & x 0 -- y is not set, but solved with constraints
          b <- point & x 4 & y 0
          v1 <- line & from a & degree 0
          onLine v1 b
          v2 <- line & from a & degree 30
          v3 <- line & from b & degree 90
          c <- intersectionPoint v2 v3
          poly [a, b, c]
    pitagoras3 `shouldBe` (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (4.0, 2.3094010767585025)]])

  it "isoceles" do
    let isoceles = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 4 & y 0
          v1 <- line & from a & degree 40
          v2 <- line & from b & degree 140
          c <- intersectionPoint v1 v2
          poly [a, b, c]
    isoceles `shouldBe` (polygon 3 [[(0.0, 0.0), (4.0, 0.0), (2.0000000000000004, 1.6781992623545603)]])
  it "chamferPita" do
    let chamferPita = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & x 4 & y 0
          v1 <- line & from a & degree 30
          v2 <- line & from b & degree 90
          c <- intersectionPoint v1 v2
          poly =<< traverse (chamfer 0.5 . pure) [a, b, c]
    chamferPita `shouldBe` ((polygon 3 [[(0.43301270189221935, 0.24999999999999994), (0.5, 0.0), (3.5, 0.0), (4.0, 0.5), (4.0, 1.8094010767585025), (3.566987298107781, 2.0594010767585025)]]))
  it "rect by relx/rely" do
    let rect = sketchPoly do
          a <- point & x 0 & y 0
          b <- point & relx a 4 & rely a 0
          c <- point & relx b 0 & rely b 4
          d <- point & relx c (-4) & rely c 0
          poly [a, b, c, d]
    rect `shouldBe` (polygon 3 [[(0, 0), (4, 0), (4, 4), (0, 4)]])
