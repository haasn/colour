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
-- |Colour operations defined by the International Commission on 
-- Illumination (CIE).
module Data.Colour.CIE
 (cieXYZ, toCIEXYZ, luminance
 ,lightness, cieLab, cieLuv
 )
where

import Data.Colour.Internal
import Data.Colour.Names
import Data.Colour.Matrix

-- |Construct a 'Colour' from XYZ coordinates for the 2° standard
-- (colourimetric) observer.
cieXYZ :: (Fractional a) => a -> a -> a -> Colour a
cieXYZ x y z = rgb709 r g b
 where
  [r,g,b] = mult (map (map fromRational) xyz2rgb) [x,y,z]

-- |Return the XYZ colour coordinates for the 2° standard
-- (colourimetric) observer.
toCIEXYZ :: (Fractional a) => Colour a -> (a,a,a)
toCIEXYZ c = (x,y,z)
 where
  (r,g,b) = toRGB709 c
  [x,y,z] = mult (map (map fromRational) rgb2xyz) [r,g,b]

rgb2xyz = [[0.412453, 0.357580, 0.180423]
          ,[0.212671, 0.715160, 0.072169]
          ,[0.019334, 0.119193, 0.950227]]

xyz2rgb = inverse rgb2xyz

{- CIE luminance -}
-- |Returns the Y colour coordinate (luminance) for the 2° standard
-- (colourimetric) observer.
luminance :: (Fractional a) => Colour a -> a
luminance c = y
 where
  (x,y,z) = toCIEXYZ c

-- |Returns the lightness of a colour, which is a perceptually uniform
-- measure.
lightness :: (Ord a, Floating a) => Colour a -> a
lightness c | (6/29)^3 < y = 116*y**(1/3) - 16
            | otherwise = (29/3)^3*y
 where
  y = luminance c

-- |Returns the CIELAB coordinates of a colour, which is a
-- perceptually uniform colour space.
cieLab :: (Ord a, Floating a) => Colour a -> (a,a,a)
cieLab c = (lightness c, a, b)
 where
  (x,y,z) = toCIEXYZ c
  (xn,yn,zn) = toCIEXYZ white
  a = 500*((x/xn)**(1/3) - (y/yn)**(1/3))
  b = 200*((y/yn)**(1/3) - (z/zn)**(1/3))

-- |Returns the CIELUV coordinates of a colour, which is a
-- perceptually uniform colour space.
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
