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
-- |An RGB space is characterised by chromaticities for red, green, and
-- blue, the chromaticity of the white point, and it's 'TransferFunction'.
module Data.Colour.RGBSpace
 ( -- *RGB Tuple
  RGB(..)
 ,uncurryRGB, curryRGB

 -- *RGB Gamut
 ,RGBGamut(..)
 ,inGamut
 -- *RGB Space
 ,TransferFunction(..)
 ,linearTransferFunction, powerTransferFunction

 ,RGBSpace(..)
 ,linearRGBSpace
 ,rgbUsingSpace
 ,toRGBUsingSpace
 )
where

import Data.Monoid
import Data.Colour
import Data.Colour.CIE.Chromaticity
import Data.Colour.Matrix
import Data.Colour.RGB
import Data.Colour.SRGB.Linear

inGamut :: (Ord b, Fractional b) => RGBGamut -> Colour b -> Bool
inGamut gamut c = r && g && b
 where
  test x = 0 <= x && x <= 1
  RGB r g b = fmap test (toRGBUsingGamut gamut c)

rtf :: (Fractional b, Real a) => [[a]] -> [[b]]
rtf = map (map realToFrac)

rgbUsingGamut :: (Fractional a) => RGBGamut -> a -> a -> a -> Colour a
rgbUsingGamut gamut r g b = rgb r0 g0 b0
 where
  matrix = rtf $ matrixMult (xyz2rgb rgbGamut) (rgb2xyz gamut)
  [r0,g0,b0] = mult matrix [r,g,b]

toRGBUsingGamut :: (Fractional a) => RGBGamut -> Colour a -> RGB a
toRGBUsingGamut gamut c = RGB r g b
 where
  RGB r0 g0 b0 = toRGB c
  matrix = rtf $ matrixMult (xyz2rgb gamut) (rgb2xyz rgbGamut)
  [r,g,b] = mult matrix [r0,g0,b0]

data TransferFunction a = TransferFunction
                          { transfer :: a -> a
                          , transferInverse :: a -> a
                          , transferGamma :: a }

linearTransferFunction :: (Num a) => TransferFunction a
linearTransferFunction = TransferFunction id id 1

powerTransferFunction :: (Floating a) => a -> TransferFunction a
powerTransferFunction gamma =
  TransferFunction (**gamma) (**(recip gamma)) gamma

instance (Num a) => Monoid (TransferFunction a) where
 mempty = linearTransferFunction
 (TransferFunction f0 f1 f) `mappend` (TransferFunction g0 g1 g) =
   (TransferFunction (f0 . g0) (g1 . f1) (f*g))

data RGBSpace a = RGBSpace { gamut :: RGBGamut,
                             transferFunction :: TransferFunction a }

linearRGBSpace :: (Num a) => RGBGamut -> RGBSpace a
linearRGBSpace gamut = RGBSpace gamut mempty

rgbUsingSpace :: (Fractional a) => RGBSpace a -> a -> a -> a -> Colour a
rgbUsingSpace space = 
  curryRGB (uncurryRGB (rgbUsingGamut (gamut space)) . fmap tinv)
 where
  tinv = transferInverse (transferFunction space)

toRGBUsingSpace :: (Fractional a) => RGBSpace a -> Colour a -> RGB a
toRGBUsingSpace space c = fmap t (toRGBUsingGamut (gamut space) c)
 where
  t = transfer (transferFunction space)
