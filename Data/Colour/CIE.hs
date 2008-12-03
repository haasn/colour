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
 (Colour
 ,cieXYZ, toCIEXYZ, luminance

 ,Chromaticity
 ,mkChromaticity, chromaCoords
 ,chromaX, chromaY, chromaZ
 ,chromaConvert
 ,chromaColour

 ,lightness, cieLab, cieLuv
 )
where

import Data.List
import Data.Colour
import Data.Colour.RGB
import Data.Colour.SRGB.Linear
import Data.Colour.CIE.Chromaticity
import Data.Colour.Matrix

-- |Construct a 'Colour' from XYZ coordinates for the 2&#176; standard
-- (colourimetric) observer.
cieXYZ :: (Fractional a) => a -> a -> a -> Colour a
cieXYZ x y z = rgb r g b
 where
  [r,g,b] = mult matrix [x,y,z]
  matrix = map (map fromRational) xyz2rgb709

-- |Return the XYZ colour coordinates for the 2&#176; standard
-- (colourimetric) observer.
toCIEXYZ :: (Fractional a) => Colour a -> (a,a,a)
toCIEXYZ c = (x,y,z)
 where
  RGB r g b = toRGB c
  [x,y,z] = mult matrix [r,g,b]
  matrix = map (map fromRational) rgb7092xyz

{- CIE luminance -}
-- |Returns the Y colour coordinate (luminance) for the 2&#176; standard
-- (colourimetric) observer.
luminance :: (Fractional a) => Colour a -> a
luminance c = y
 where
  (x,y,z) = toCIEXYZ c

instance AffineSpace Chromaticity where
 affineCombo l z =
   foldl1' chromaAdd [chromaScale w a | (w,a) <- (1-total,z):l]
  where
   total = sum $ map fst l
   (Chroma x0 y0) `chromaAdd` (Chroma x1 y1) = Chroma (x0+x1) (y0+y1)
   s `chromaScale` (Chroma x y) = Chroma (s*x) (s*y)

-- |Constructs a colour from the given 'Chromaticity' and 'luminance'.
chromaColour :: (Fractional a) => Chromaticity a -> a -> Colour a
chromaColour ch y = cieXYZ (s*ch_x) y (s*ch_z)
 where
  (ch_x, ch_y, ch_z) = chromaCoords ch
  s = y/ch_y

-- |Returns the lightness of a colour, which is a perceptually uniform
-- measure.
lightness :: (Ord a, Floating a) => Colour a -> a
lightness c | (6/29)^3 < y = 116*y**(1/3) - 16
            | otherwise = (29/3)^3*y
 where
  y = luminance c

-- |Returns the CIELAB coordinates of a colour, which is a
-- perceptually uniform colour space.
-- If you don't know what white point to use, use
-- 'Data.Colour.CIE.Illuminant.d65'.
cieLab :: (Ord a, Floating a) => Chromaticity a -- ^White point
                              -> Colour a -> (a,a,a)
cieLab white_ch c = (lightness c, a, b)
 where
  white = chromaColour white_ch 1.0
  (x,y,z) = toCIEXYZ c
  (xn,yn,zn) = toCIEXYZ white
  a = 500*((x/xn)**(1/3) - (y/yn)**(1/3))
  b = 200*((y/yn)**(1/3) - (z/zn)**(1/3))

-- |Returns the CIELUV coordinates of a colour, which is a
-- perceptually uniform colour space.
-- If you don't know what white point to use, use
-- 'Data.Colour.CIE.Illuminant.d65'.
cieLuv :: (Ord a, Floating a) => Chromaticity a -- ^White point
                              -> Colour a -> (a,a,a)
cieLuv white_ch c = (l, 13*l*(u'-un'), 13*l*(v'-vn'))
 where
  white = chromaColour white_ch 1.0
  (u', v') = u'v' c
  (un', vn') = u'v' white
  l = lightness c
--------------------------------------------------------------------------
{- not for export -}
u'v' :: (Ord a, Floating a) => Colour a -> (a,a)
u'v' c = (4*x/(x+15*y+3*z), 9*y/(x+15*y+3*z))
 where
  (x,y,z) = toCIEXYZ c

rgb7092xyz = (rgb2xyz rgbGamut)

xyz2rgb709 = inverse rgb7092xyz

