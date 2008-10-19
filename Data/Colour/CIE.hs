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
module Data.Colour.CIE
 (cieXYZ, toCIEXYZ, luminance
 ,lightness, cieLab, cieLuv
 )
where

import Data.Colour
import Data.Colour.Names

cieXYZ :: (Fractional a) => a -> a -> a -> Colour a
cieXYZ x y z = rgb709 r g b
 where
  r = transformBy [ 3.240479, -1.537150, -0.498535]
  g = transformBy [-0.969256,  1.875992,  0.041556]
  b = transformBy [ 0.055648, -0.204043,  1.057311]
  transformBy l = sum $ zipWith (*) l [x,y,z]

toCIEXYZ :: (Fractional a) => Colour a -> (a,a,a)
toCIEXYZ c = (x,y,z)
 where
  (r,g,b) = toRGB709 c
  x = transformBy [0.412453, 0.357580, 0.180423]
  y = transformBy [0.212671, 0.715160, 0.072169]
  z = transformBy [0.019334, 0.119193, 0.950227]
  transformBy l = sum $ zipWith (*) l [r,g,b]

{- CIE luminance -}
luminance :: (Fractional a) => Colour a -> a
luminance c = y
 where
  (x,y,z) = toCIEXYZ c

lightness :: (Ord a, Floating a) => Colour a -> a
lightness c | 0.008856 < y = 116*y**(1/3) - 16
            | otherwise = 903.3*y
 where
  y = luminance c

cieLab :: (Ord a, Floating a) => Colour a -> (a,a,a)
cieLab c = (lightness c, a, b)
 where
  (x,y,z) = toCIEXYZ c
  (xn,yn,zn) = toCIEXYZ white
  a = 500*((x/xn)**(1/3) - (y/yn)**(1/3))
  b = 200*((y/yn)**(1/3) - (z/zn)**(1/3))

cieLuv :: (Ord a, Floating a) => Colour a -> (a,a,a)
cieLuv c = (l, 13*l*(u'-un'), 13*l*(v'-vn'))
 where
  (u', v') = u'v' c
  (un', vn') = u'v' white
  l = lightness c
--------------------------------------------------------------------------
{- not for export -}
u'v' :: (Ord a, Floating a) => Colour a -> (a,a)
u'v' c = (4*x/(x+15*y+3*z), 9*y/(x+15*y+3*z))
 where
  (x,y,z) = toCIEXYZ c
