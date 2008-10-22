{-# OPTIONS_GHC -XTypeSynonymInstances #-}
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
module Main where

import Data.Word
import Control.Monad
import Test.QuickCheck
import Text.Printf

import Data.Colour
import Data.Colour.SRGB
import Data.Colour.CIE
import Data.Colour.Names
import Data.Colour.HDTV as HDTV
import qualified Data.Colour.SDTV as SDTV

default (Rational, Double, Float)

type RColour = Colour Rational
type DColour = Colour Double
type FColour = Colour Float
type RAlphaColour = AlphaColour Rational
type DAlphaColour = AlphaColour Double

instance Arbitrary Word8 where
  arbitrary = liftM fromIntegral $
              choose (fromIntegral (minBound::Word8)::Int,
                      fromIntegral (maxBound::Word8)::Int)
  coarbitrary x = variant (fromIntegral x)

instance Arbitrary (Rational) where
  arbitrary = liftM (toRational :: Double -> Rational) $ arbitrary
  coarbitrary x = coarbitrary (fromRational x :: Double)

instance (Real a, Fractional a, Arbitrary a) => Arbitrary (Colour a) where
  arbitrary = liftM3 mkColour arbitrary arbitrary arbitrary
   where
    mkColour r' g' b' = colourConvert (sRGB24 r' g' b'::Colour Double)
  coarbitrary = coarbitrary . toRGB709

instance (Real a, Fractional a, Arbitrary a) =>
         Arbitrary (AlphaColour a) where
  arbitrary = liftM2 mkAlphaColour arbitrary arbitrary
   where
    mkAlphaColour :: (Fractional a) => Colour a -> Word8 -> AlphaColour a
    mkAlphaColour c a =
      c `withOpacity` (fromIntegral a/fromIntegral (maxBound `asTypeOf` a))
  coarbitrary ac | a == 0 = coarbitrary a
                | otherwise = coarbitrary a . coarbitrary c
   where
    a = alphaChannel ac
    c = colourChannel ac

prop_toFromRGB709 :: RColour -> Bool
prop_toFromRGB709 c = (rgb709 r g b) == c
 where
  (r,g,b) = toRGB709 c

prop_fromToRGB709 :: Rational -> Rational -> Rational -> Bool
prop_fromToRGB709 r g b = toRGB709 (rgb709 r g b) == (r,g,b)

prop_toFromXYZ :: RColour -> Bool
prop_toFromXYZ c = (cieXYZ x y z) == c
 where
  (x,y,z) = toCIEXYZ c

prop_fromToXYZ :: Rational -> Rational -> Rational -> Bool
prop_fromToXYZ x y z = toCIEXYZ (cieXYZ x y z) == (x,y,z)

-- Uses the fact that an Arbitrary colour is an sRGB24 colour.
prop_toFromSRGB :: DColour -> Bool
prop_toFromSRGB c = (sRGB24 r' g' b') == c
 where
  (r',g',b') = toSRGB24 c

prop_fromToSRGB :: Word8 -> Word8 -> Word8 -> Bool
prop_fromToSRGB r' g' b' = toSRGB24 (sRGB24 r' g' b') == (r',g',b')

prop_fromToY'CbCr709 :: Word8 -> Word8 -> Word8 -> Bool
prop_fromToY'CbCr709 y' cb cr =
  HDTV.toY'CbCr (HDTV.y'CbCr y' cb cr) == (y',cb,cr)

prop_fromToY'CbCr601 :: Word8 -> Word8 -> Word8 -> Bool
prop_fromToY'CbCr601 y' cb cr =
  SDTV.toY'CbCr (SDTV.y'CbCr y' cb cr) == (y',cb,cr)

prop_transparentOver :: RColour -> Bool
prop_transparentOver c = transparent `over` c == c

prop_overTransparent :: RAlphaColour -> Bool
prop_overTransparent c = c `over` transparent == c

prop_opaqueOver :: RColour -> RColour -> Bool
prop_opaqueOver c1 c2 = opaque c1 `over` c2 == c1

prop_blendOver :: Rational -> RColour -> RColour -> Bool
prop_blendOver o c1 c2 = 
  (c1 `withOpacity` o) `over` c2 == blend o c1 c2

prop_blendTransparent :: Rational -> Rational -> RColour -> Bool
prop_blendTransparent o a c = 
  blend o (c `withOpacity` a) transparent == c `withOpacity ` (o*a)

prop_sRGB24showlength :: DColour -> Bool
prop_sRGB24showlength c = length (sRGB24show c) == 7

prop_readshowSRGB24 :: DColour -> Bool
prop_readshowSRGB24 c =
  sRGB24show (sRGB24read (sRGB24show c)) == sRGB24show c

prop_luminance_white :: Bool
prop_luminance_white = luminance white == 1

tests = [("RGB709-to-from", test prop_toFromRGB709)
        ,("RGB709-from-to", test prop_fromToRGB709)
        ,("XYZ-to-from", test prop_toFromXYZ)
        ,("XYZ-from-to", test prop_fromToXYZ)
        ,("sRGB-to-from", test prop_toFromSRGB)
        ,("sRGB-from-to", test prop_fromToSRGB)
        ,("Y'CbCr-709-from-to", test prop_fromToY'CbCr709)
        ,("Y'CbCr-601-from-to", test prop_fromToY'CbCr709)
        ,("transparent-over", test prop_transparentOver)
        ,("over-transparent", test prop_overTransparent)
        ,("opaque-over", test prop_opaqueOver)
        ,("blend-over", test prop_blendOver)
        ,("blend-transparent", test prop_blendTransparent)
        ,("sRGB24-show-length", test prop_sRGB24showlength)
        ,("sRGB24-read-show", test prop_readshowSRGB24)
        ,("luminance-white", check defaultConfig{configMaxTest = 1}
                             prop_luminance_white)
        ]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
