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
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.Internal (quantize)
import Data.Word

type LumaCoef = (Rational, Rational, Rational)

luma :: (Ord a, Floating a) => LumaCoef -> (Colour a -> RGB a)
                                        -> Colour a -> a
luma (lr, lg, lb) toRGB c =
  transformBy [fromRational lr, fromRational lg, fromRational lb]
 where
  RGB r' g' b' = toRGB c
  transformBy l = sum $ zipWith (*) l [r',g',b']

y'PbPr :: (Ord a, Floating a) => LumaCoef -> (a -> a -> a -> Colour a) 
                                          -> a -> a -> a -> Colour a
y'PbPr (lr, lg, lb) rgb y' pb pr = rgb r' g' b'
 where
  r' = y' + fromRational ((lg + lb)/0.5)*pr
  g' = (y' - fromRational lr*r' - fromRational lb*b')/fromRational lg
  b' = y' + fromRational ((lg + lr)/0.5)*pb

toY'PbPr :: (Ord a, Floating a) => LumaCoef -> (Colour a -> RGB a)
                                            -> Colour a -> (a, a, a)
toY'PbPr l@(lr, lg, lb) toRGB c = (y', pb, pr)
 where
  y' = luma l toRGB c
  RGB r' g' b' = toRGB c
  pb = fromRational (0.5/(lg + lr))*(b' - y')
  pr = fromRational (0.5/(lg + lb))*(r' - y')

y'CbCr :: (Floating a, RealFrac a) =>
          LumaCoef -> (a -> a -> a -> Colour a) 
                   -> Word8 -> Word8 -> Word8 -> Colour a
y'CbCr l rgb y' cb cr = y'PbPr l rgb y'0 pb pr
 where
  y'0 = ((fromIntegral y') - 16)/219
  pb  = ((fromIntegral cb) - 128)/224
  pr  = ((fromIntegral cr) - 128)/224

toY'CbCr :: (Floating a, RealFrac a) =>
            LumaCoef -> (Colour a -> RGB a)
                     -> Colour a -> (Word8, Word8, Word8)
toY'CbCr l toRGB c = (quantize $ 16 + 219*y'
                     ,quantize $ 128 + 224*pb
                     ,quantize $ 128 + 224*pr)
 where
  (y', pb, pr) = toY'PbPr l toRGB c

r'g'b' :: (Floating a, RealFrac a) =>
          (a -> a -> a -> Colour a) ->
          Word8 -> Word8 -> Word8 -> Colour a
r'g'b' rgb r' g' b' = rgb (f r') (f g') (f b')
 where
  f x' = ((fromIntegral x') - 16)/219

toR'G'B' :: (Floating a, RealFrac a) =>
            (Colour a -> RGB a) ->
            Colour a -> RGB Word8
toR'G'B' toRGB c = fmap f (toRGB c)
 where
  f x' = quantize $ 16 + 219*x'
