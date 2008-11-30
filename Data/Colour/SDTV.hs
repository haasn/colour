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
-- |Defines the Y'CbCr and Y'PbPr colour spaces in accordance with
-- ITU-R Recommendation BT.601 used for standard definition television
-- (SDTV).
--
-- For high definition television (HDTV) see "Data.Colour.HDTV".
module Data.Colour.SDTV
 (luma
 ,y'PbPr, toY'PbPr
 ,y'CbCr, toY'CbCr
 )
where

import Data.Word
import Data.Colour
import Data.Colour.RGBSpace
import qualified Data.Colour.Luma as L

{- #TODO# ALL BROKEN  NEEDS TO BE NONLINEAR!!! -}
{- rec 601 luma -}
-- |Luma (Y') approximates the 'Data.Colour.CIE.lightness' of a 'Colour'.
luma :: (Ord a, Floating a) => RGBSpace a
                            -> Colour a -> a
luma space = L.luma lumaCoef (toRGBUsingSpace space)

-- |Construct a 'Colour' from Y'PbPr coordinates.
y'PbPr :: (Ord a, Floating a) => RGBSpace a
                              -> a -> a -> a -> Colour a
y'PbPr space = L.y'PbPr lumaCoef (rgbUsingSpace space)

-- |Returns the Y'PbPr coordinates of a 'Colour'.
toY'PbPr :: (Ord a, Floating a) => RGBSpace a
                                -> Colour a -> (a, a, a)
toY'PbPr space = L.toY'PbPr lumaCoef (toRGBUsingSpace space)

-- |Construct a 'Colour' from Y'CbRr 8-bit coordinates.
y'CbCr :: (Floating a, RealFrac a) => RGBSpace a
                                   -> Word8 -> Word8 -> Word8 -> Colour a
y'CbCr space = L.y'CbCr lumaCoef (rgbUsingSpace space)

-- |Returns the Y'CbCr 8-bit coordinates of a 'Colour'.
toY'CbCr :: (Floating a, RealFrac a) => RGBSpace a
                                     -> Colour a -> (Word8, Word8, Word8)
toY'CbCr space = L.toY'CbCr lumaCoef (toRGBUsingSpace space)

{- Not for export -}
lumaCoef = (0.299, 0.587, 0.114)