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
 (RGBSpace(..)
 ,rgbSpace
 ,toRGBSpace
 )
where

import Data.Colour.CIE.Chromaticity
import Data.Colour.Internal
import Data.Colour.Matrix

rgbSpace :: (Fractional a) => RGBSpace a -> a -> a -> a -> Colour a
rgbSpace space r g b = rgb709 r0 g0 b0
 where
  matrix = matrixMult (xyz2rgb rgb709Space) (rgb2xyz space)
  [r0,g0,b0] = mult matrix [r,g,b]

toRGBSpace :: (Fractional a) => RGBSpace a -> Colour a -> (a,a,a)
toRGBSpace space c = (r,g,b)
 where
  (r0,g0,b0) = toRGB709 c
  matrix = matrixMult (xyz2rgb space) (rgb2xyz rgb709Space)
  [r,g,b] = mult matrix [r0,g0,b0]

