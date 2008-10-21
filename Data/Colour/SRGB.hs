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
module Data.Colour.SRGB
 (sRGB24, sRGBBounded, sRGB
 ,toSRGB24, toSRGBBounded, toSRGB

 ,sRGB24shows, sRGB24show
 ,sRGB24reads, sRGB24read
  --,transferFunction, invTransferFunction -- should these be exported?
 )
where

import Data.Word
import Numeric
import Data.Colour.Internal

{- Non-linear colour space -}
{- the sRGB transfer function approximates a gamma of about 2.2 -}
transferFunction lin | lin <= 0.0031308 = 12.92*lin
                            | otherwise        = (1 + a)*lin**(1/2.4) - a
 where
  a = 0.055

invTransferFunction nonLin | nonLin <= 0.04045 =
 nonLin/12.92
                           | otherwise         =
 ((nonLin + a)/(1 + a))**2.4
 where
  a = 0.055

sRGB :: (Ord b, Floating b) =>  b -> b -> b -> Colour b
sRGB r' g' b' = rgb709 r g b
 where
  r = invTransferFunction r'
  g = invTransferFunction g'
  b = invTransferFunction b'

sRGBBounded :: (Ord b, Floating b, Integral a, Bounded a) =>
               a -> a -> a -> Colour b
sRGBBounded r' g' b' = sRGB (fromIntegral r'/m)
                            (fromIntegral g'/m)
                            (fromIntegral b'/m)
 where
  m = fromIntegral $ maxBound `asTypeOf` r'

sRGB24 :: (Ord b, Floating b) => Word8 -> Word8 -> Word8 -> Colour b
sRGB24 = sRGBBounded

toSRGB :: (RealFrac b, Floating b) => Colour b -> (b, b, b)
toSRGB c = (r', g', b')
 where
  (r,g,b) = toRGB709 c
  r' = transferFunction r
  g' = transferFunction g
  b' = transferFunction b

{- Results are clamped and quantized -}

toSRGBBounded :: (RealFrac b, Floating b, Integral a, Bounded a) =>
                 Colour b -> (a,a,a)
toSRGBBounded c = (r', g', b')
 where
  (r'0, g'0, b'0) = toSRGB c
  (r', g', b') = (quantize (m*r'0), quantize (m*g'0), quantize (m*b'0))
  m = fromIntegral $ maxBound `asTypeOf` r'

toSRGB24 :: (RealFrac b, Floating b) => Colour b -> (Word8, Word8, Word8)
toSRGB24 = toSRGBBounded

sRGB24shows :: (RealFrac b, Floating b) => Colour b -> ShowS
sRGB24shows c =
  ("#"++) . showHex2 r' . showHex2 g' . showHex2 b'
 where
  (r', g', b') = toSRGB24 c
  showHex2 x | x <= 0xf = ("0"++) . showHex x
             | otherwise = showHex x

sRGB24show :: (RealFrac b, Floating b) => Colour b -> String
sRGB24show x = sRGB24shows x ""

sRGB24reads :: (RealFrac b, Floating b) => ReadS (Colour b)
sRGB24reads "" = []
sRGB24reads x =
  [(sRGB24 a b c, c0)
  |(a,a0) <- readPair x', (b,b0) <- readPair a0, (c,c0) <- readPair b0]
 where
  x' | head x == '#' = tail x
     | otherwise = x
  readPair [] = []
  readPair [_] = []
  readPair a = [(x,a1)|(x,"") <- readHex a0]
   where
    (a0,a1) = splitAt 2 a

sRGB24read :: (RealFrac b, Floating b) => String -> (Colour b)
sRGB24read x | length rx /= 1 || not (null (snd (head rx))) =
  error "Data.Colour.SRGB.readSRGB24: no parse"
             | otherwise = fst (head rx)
 where
  rx = sRGB24reads x
