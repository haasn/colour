{-
Copyright (c) 2008, 2013
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
-- |Defines the Y'CbCr and Y'PbPr colour spaces in accordance with
-- ITU-R Recommendation BT.601 used for 525-line (North America) standard
-- definition television (SDTV).
--
-- For high definition television (HDTV) see "Data.Colour.HDTV".
module Data.Colour.SDTV625
{-
 (Colour
 ,luma
 ,y'PbPr, toY'PbPr
 ,y'CbCr, toY'CbCr
 )
 -}
where

import Data.Word
import Data.Colour.RGBSpace
import Data.Colour.SRGB (sRGBSpace)
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.CIE
import Data.Colour.SDTV
import qualified Data.Colour.Luma as L

space :: (Ord a, Floating a) => RGBSpace a
space = mkRGBSpace gamut transfer
 where
  gamut = mkRGBGamut (RGB (mkChromaticity 0.64 0.33)
                          (mkChromaticity 0.29 0.60)
                          (mkChromaticity 0.15 0.06))
                     d65
  transfer = transferFunction sRGBSpace
  
{- rec 601 luma -}
-- |Luma (Y') approximates the 'Data.Colour.CIE.lightness' of a 'Colour'.
luma :: (Ord a, Floating a) => Colour a -> a
luma = L.luma lumaCoef space

-- |Construct a 'Colour' from Y'PbPr coordinates.
y'PbPr :: (Ord a, Floating a) => a -> a -> a -> Colour a
y'PbPr = L.y'PbPr lumaCoef space

-- |Returns the Y'PbPr coordinates of a 'Colour'.
toY'PbPr :: (Ord a, Floating a) => Colour a -> (a, a, a)
toY'PbPr = L.toY'PbPr lumaCoef space

-- |Construct a 'Colour' from Y'CbRr studio 8-bit coordinates.
y'CbCr :: (Floating a, RealFrac a) => Word8 -> Word8 -> Word8 -> Colour a
y'CbCr = L.y'CbCr lumaCoef space

-- |Returns the Y'CbCr studio 8-bit coordinates of a 'Colour'.
toY'CbCr :: (Floating a, RealFrac a) => Colour a -> (Word8, Word8, Word8)
toY'CbCr = L.toY'CbCr lumaCoef space

-- |Construct a 'Colour' from R'G'B' studio 8-bit coordinates.
r'g'b' :: (Floating a, RealFrac a) => Word8 -> Word8 -> Word8 -> Colour a
r'g'b' = L.r'g'b' space

-- |Returns the Y'CbCr studio 8-bit coordinates of a 'Colour'.
toR'G'B' :: (Floating a, RealFrac a) => Colour a -> RGB Word8
toR'G'B' = L.toR'G'B' space
