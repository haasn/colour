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
module Data.Colour.CIE.Chromaticity where

import Data.Colour.Matrix

data Chromaticity a = Chroma !a !a deriving (Eq)

-- |Returns the CIE little /x/, little /y/, little /z/ coordinates
-- for the 2° standard (colourimetric) observer.
chroma_coords :: (Fractional a) => Chromaticity a -> (a, a, a)
chroma_coords (Chroma x y) = (x, y, 1 - x - y)

-- |Constructs 'Chromaticity' from the CIE little /x/, little /y/
-- coordinates for the 2° standard (colourimetric) observer.
cieChroma :: (Fractional a) => a -> a -> Chromaticity a
cieChroma = Chroma

-- Should a always be Rational?
data RGBSpace a = RGBSpace {primaryRed   :: Chromaticity a
                           ,primaryGreen :: Chromaticity a
                           ,primaryBlue  :: Chromaticity a
                           ,whitePoint   :: Chromaticity a
                           } deriving (Eq)

{- not for export -}
rgb2xyz :: (Fractional a) => RGBSpace a -> [[a]]
rgb2xyz space =[[ar*xr, ag*xg, ab*xb]
               ,[ar*yr, ag*yg, ab*yb]
               ,[ar*zr, ag*zg, ab*zb]]
 where
  (xr, yr, zr) = chroma_coords (primaryRed   space)
  (xg, yg, zg) = chroma_coords (primaryGreen space)
  (xb, yb, zb) = chroma_coords (primaryBlue  space)
  (xn, yn, zn) = chroma_coords (whitePoint space)
  matrix = [[xr, xg, xb]
           ,[yr, yg, yb]
           ,[zr, zg, zb]]
  [ar, ag, ab] = mult (inverse matrix) [xn/yn, 1, zn/yn]

xyz2rgb :: (Fractional a) => RGBSpace a -> [[a]]
xyz2rgb = inverse . rgb2xyz