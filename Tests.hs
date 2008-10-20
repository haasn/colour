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
import Data.Colour.Rec709

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

prop_toFromRGB709 :: RColour -> Bool
prop_toFromRGB709 c = (rgb709 r g b) == c
 where
  (r,g,b) = toRGB709 c

prop_toFromXYZ :: FColour -> Bool
prop_toFromXYZ c = (cieXYZ x y z) == c
 where
  (x,y,z) = toCIEXYZ c

prop_sRGB24showlength :: DColour -> Bool
prop_sRGB24showlength c = length (sRGB24show c) == 7

prop_readshowSRGB24 :: DColour -> Bool
prop_readshowSRGB24 c =
  sRGB24show (sRGB24read (sRGB24show c)) == sRGB24show c

prop_transparentOver :: RColour -> Bool
prop_transparentOver c = transparent `over` c == c

prop_overTransparent :: RAlphaColour -> Bool
prop_overTransparent c = c `over` transparent == c

prop_opaqueOver :: RColour -> RColour -> Bool
prop_opaqueOver c1 c2 = alphaColour c1 `over` c2 == c1

tests = [("RGB709-to-from", test prop_toFromRGB709)
--        ,("XYZ-to-from", test prop_toFromXYZ)
        ,("sRGB24-show-length", test prop_sRGB24showlength)
        ,("sRGB24-read-show", test prop_readshowSRGB24)
        ,("transparent-over", test prop_transparentOver)
        ,("over-transparent", test prop_overTransparent)
        ,("opaque-over", test prop_opaqueOver)
        ]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
