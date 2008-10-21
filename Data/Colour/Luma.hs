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
module Data.Colour.Luma where
{- For internal use only:
   Not to be exported from the package -}
import Data.Colour.SRGB
import Data.Colour.Internal
import Data.Word

type LumaCoef = (Rational, Rational, Rational)

{- rec 709 luma -}
luma :: (Floating a, RealFrac a) => LumaCoef -> Colour a -> a
luma (lr, lg, lb) c =
  transformBy [fromRational lr, fromRational lr, fromRational lr]
 where
  (r',g',b') = toSRGB c
  transformBy l = sum $ zipWith (*) l [r',g',b']

y'PbPr :: (Floating a, RealFrac a) => LumaCoef -> a -> a -> a -> Colour a
y'PbPr (lr, lg, lb) y' pb pr = sRGB r' g' b'
 where
  r' = y' + fromRational ((lg + lb)/0.5)*pr
  g' = (y' - fromRational lr*r' - fromRational lb*b')/fromRational lg
  b' = y' + fromRational ((lg + lr)/0.5)*pb

toY'PbPr :: (Floating a, RealFrac a) => LumaCoef -> Colour a -> (a, a, a)
toY'PbPr l@(lr, lg, lb) c = (y', pb, pr)
 where
  y' = luma l c
  (r', g', b') = toSRGB c
  pb = fromRational (0.5/(lg + lr))*(b' - y')
  pr = fromRational (0.5/(lg + lb))*(r' - y')

y'CbCr :: (Floating a, RealFrac a) =>
          LumaCoef -> Word8 -> Word8 -> Word8 -> Colour a
y'CbCr l y' cb cr = y'PbPr l y'0 pb pr
 where
  y'0 = ((fromIntegral y') - 16)/219
  pb  = ((fromIntegral cb) - 128)/224
  pr  = ((fromIntegral cr) - 128)/224

toY'CbCr :: (Floating a, RealFrac a) =>
            LumaCoef -> Colour a -> (Word8, Word8, Word8)
toY'CbCr l c = (quantize $ 16 + 219*y'
             ,quantize $ 128 + 224*pb
             ,quantize $ 128 + 224*pr)
 where
  (y', pb, pr) = toY'PbPr l c
