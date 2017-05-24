module Main where

import Prelude
import Flare.Drawing as D
import Graphics.Isometric.Point as P
import Color (Color, darken, rgb')
import Color (Color, rgb)
import Color.Scheme.MaterialDesign (blue, red, purple, pink, yellow, grey)
import Control.Alternative (pure)
import Data.Array (range, (..))
import Data.Foldable (foldMap, fold)
import Data.Int (floor, toNumber)
import Flare (numberSlider, lift, intSlider, color)
import Graphics.Isometric (Point, cube, filled, rotateZ, scale, renderScene, prism, translateX, translateY, cone)
import Graphics.Isometric.DepthSort (depthSort)
import Math (sin, cos, pi)
import Signal.DOM (animationFrame)

-- Based upon purescript-isometric Example 3

p :: Int -> Int -> Int -> Point
p x y z = { x: toNumber x, y: toNumber y, z: toNumber z }

size = 6.0
intSize = floor size

coloring {x, y, z} = rgb' (z / size) (y / size) (x / size)

colorcube :: Number -> Int -> Int -> Int -> D.Drawing
colorcube angle sliceX sliceY sliceZ =
  D.translate 300.0 300.0 $
    renderScene { x: -4.0, y: -1.0, z: 3.0 } $
      scale 45.0 $ rotateZ angle $
        foldMap (\pos -> filled (coloring pos) (cube pos 1.0))
        do
          x <- range 1 sliceX
          y <- range 1 sliceY
          z <- range 1 sliceZ
          pure (p x y z)

main = do
   D.runFlareDrawing "controls3" "canvas3" $
    colorcube <$> numberSlider "Rotation" (-0.25 * pi) (0.25 * pi) 0.01 0.0
      <*> intSlider "Slice X" 1 intSize intSize
      <*> intSlider "Slice Y" 1 intSize intSize
      <*> intSlider "Slice Z" 1 intSize intSize
