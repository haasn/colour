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
import Data.Colour.CIE (luminance)
import Data.Word
import Control.Applicative

type LumaCoef = RGB Rational

lumaCoef :: RGBGamut -> LumaCoef
lumaCoef gamut = RGB lr lg lb
 where
  space = linearRGBSpace gamut
  lr = luminance (rgbUsingSpace space 1 0 0)
  lg = luminance (rgbUsingSpace space 0 1 0)
  lb = luminance (rgbUsingSpace space 0 0 1)

luma :: (Ord a, Floating a) => LumaCoef -> RGBSpace a -> Colour a -> a
luma lumaCoef space c = rv + gv + bv
 where
  RGB rv gv bv = liftA2 (*) (toRGBUsingSpace space c) (fromRational <$> lumaCoef)

y'PbPr :: (Ord a, Floating a) => LumaCoef -> RGBSpace a -> a -> a -> a -> Colour a
y'PbPr (RGB lr lg lb) space y' pb pr = rgbUsingSpace space r' g' b'
 where
  r' = y' + fromRational ((lg + lb)/0.5)*pr
  g' = (y' - fromRational lr*r' - fromRational lb*b')/fromRational lg
  b' = y' + fromRational ((lg + lr)/0.5)*pb

toY'PbPr :: (Ord a, Floating a) => LumaCoef -> RGBSpace a -> Colour a -> (a, a, a)
toY'PbPr lumaCoef@(RGB lr lg lb) space c = (y', pb, pr)
 where
  RGB r' g' b' = toRGBUsingSpace space c
  y' = luma lumaCoef space c
  pb = fromRational (0.5/(lg + lr))*(b' - y')
  pr = fromRational (0.5/(lg + lb))*(r' - y')

y'CbCr :: (Floating a, RealFrac a) =>
          LumaCoef -> RGBSpace a -> Word8 -> Word8 -> Word8 -> Colour a
y'CbCr lumaCoef space y' cb cr = y'PbPr lumaCoef space y'0 pb pr
 where
  y'0 = ((fromIntegral y') - 16)/219
  pb  = ((fromIntegral cb) - 128)/224
  pr  = ((fromIntegral cr) - 128)/224

toY'CbCr :: (Floating a, RealFrac a) =>
            LumaCoef -> RGBSpace a -> Colour a -> (Word8, Word8, Word8)
toY'CbCr lumaCoef space c = (quantize $ 16 + 219*y'
                            ,quantize $ 128 + 224*pb
                            ,quantize $ 128 + 224*pr)
 where
  (y', pb, pr) = toY'PbPr lumaCoef space c

r'g'b' :: (Floating a, RealFrac a) =>
          RGBSpace a -> Word8 -> Word8 -> Word8 -> Colour a
r'g'b' space r' g' b' = rgbUsingSpace space (f r') (f g') (f b')
 where
  f x' = ((fromIntegral x') - 16)/219

toR'G'B' :: (Floating a, RealFrac a) =>
            RGBSpace a -> Colour a -> RGB Word8
toR'G'B' space c = fmap f (toRGBUsingSpace space c)
 where
  f x' = quantize $ 16 + 219*x'
  
{- The commonly used transfer function fror HDTV and SDTV -}
transferFunction :: (Ord a, Floating a) => TransferFunction a
transferFunction = TransferFunction tf invtf 0.5 -- Digital Video and HDTV Algorithims and Interfaces, page. 264
 where
  tf x | x < 0.0018 = 4.5 * x
       | otherwise = 1.099 * x**(0.45) - 0.099
  invtf x | x < 0.0018*4.5 = x / 4.5
	  | otherwise = (x**(recip 0.45)/1.099) + 0.099