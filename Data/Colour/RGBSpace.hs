{-
Copyright (c) 2008
Russell O'Connor

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
-- |An RGB space is characterised by chromaticities for red, green, and
-- blue, and the chromaticity of the white point.
module Data.Colour.RGBSpace
 (RGB(..)
 ,uncurryRGB, curryRGB

 ,RGBSpace(..)
 ,rgbUsingSpace
 ,toRGBUsingSpace
 )
where

import Data.Colour
import Data.Colour.CIE.Chromaticity
import Data.Colour.Matrix
import Data.Colour.RGB
import Data.Colour.SRGB.Linear

rgbUsingSpace :: (Fractional a) => RGBSpace a -> a -> a -> a -> Colour a
rgbUsingSpace space r g b = rgb r0 g0 b0
 where
  matrix = matrixMult (xyz2rgb rgbSpace) (rgb2xyz space)
  [r0,g0,b0] = mult matrix [r,g,b]

toRGBUsingSpace :: (Fractional a) => RGBSpace a -> Colour a -> RGB a
toRGBUsingSpace space c = RGB r g b
 where
  RGB r0 g0 b0 = toRGB c
  matrix = matrixMult (xyz2rgb space) (rgb2xyz rgbSpace)
  [r,g,b] = mult matrix [r0,g0,b0]

