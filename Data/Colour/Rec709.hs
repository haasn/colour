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
module Data.Colour.Rec709
 (luma
 ,y'PbPr, toY'PbPr
 ,y'CbCr, toY'CbCr
 )
where

import Data.Word
import Data.Colour
import qualified Data.Colour.Luma as L

{- rec 601 luma -}
luma :: (Floating a, RealFrac a) => Colour a -> a
luma = L.luma lumaCoef

y'PbPr :: (Floating a, RealFrac a)  => a -> a -> a -> Colour a
y'PbPr = L.y'PbPr lumaCoef

toY'PbPr :: (Floating a, RealFrac a)  => Colour a -> (a, a, a)
toY'PbPr = L.toY'PbPr lumaCoef

y'CbCr :: (Floating a, RealFrac a)  => Word8 -> Word8 -> Word8 -> Colour a
y'CbCr = L.y'CbCr lumaCoef

toY'CbCr :: (Floating a, RealFrac a)  => Colour a -> (Word8, Word8, Word8)
toY'CbCr = L.toY'CbCr lumaCoef

{- Not for export -}
lumaCoef = (0.2126, 0.7152, 0.0722)