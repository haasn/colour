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
module Data.Colour.RGB where

import Data.List
import Data.Colour.Matrix
import Data.Colour.CIE.Chromaticity

-- |An RGB triple for an unspecified colour space.
data RGB a = RGB {channelRed :: !a
                 ,channelGreen :: !a
                 ,channelBlue :: !a
                 } deriving (Eq, Show, Read)

instance Functor RGB where
 fmap f (RGB r g b) = RGB (f r) (f g) (f b)

-- |Uncurries a function expecting three r, g, b parameters.
uncurryRGB :: (a -> a -> a -> b) -> RGB a -> b
uncurryRGB f (RGB r g b) = f r g b

-- |Curries a function expecting one RGB parameter.
curryRGB :: (RGB a -> b) -> a -> a -> a -> b
curryRGB f r g b = f (RGB r g b)

-- Should a always be Rational?
data RGBGamut a = RGBGamut {primaries :: !(RGB (Chromaticity a))
                           ,whitePoint   :: !(Chromaticity a)
                           } deriving (Eq, Read, Show)

{- not for export -}

gamutConvert :: (Fractional b, Real a) => RGBGamut a -> RGBGamut b
gamutConvert (RGBGamut p w) =
  RGBGamut (fmap chromaConvert p) (chromaConvert w)

primaryMatrix :: (Fractional a) => (RGB (Chromaticity a)) -> [[a]]
primaryMatrix p =
  [[xr, xg, xb]
  ,[yr, yg, yb]
  ,[zr, zg, zb]]
 where
  RGB (xr, yr, zr)
      (xg, yg, zg)
      (xb, yb, zb) = fmap chroma_coords p

rgb2xyz :: (Fractional a) => RGBGamut a -> [[a]]
rgb2xyz space =
  transpose (zipWith (map . (*)) as (transpose matrix))
 where
  (xn, yn, zn) = chroma_coords (whitePoint space)
  matrix = primaryMatrix (primaries space)
  as = mult (inverse matrix) [xn/yn, 1, zn/yn]

xyz2rgb :: (Fractional a) => RGBGamut a -> [[a]]
xyz2rgb = inverse . rgb2xyz
