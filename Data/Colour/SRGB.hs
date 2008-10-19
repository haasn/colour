module Data.Colour.SRGB
 (sRGBranged, sRGB, sRGB24
 ,toSRGBranged, toSRGB, toSRGB24

 ,sRGB24shows, sRGB24show
 ,sRGB24reads, sRGB24read
  --,transferFunction, invTransferFunction -- should these be exported?
 )
where

import Data.Word
import Numeric
import Data.Colour

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

sRGBranged :: (Ord b, Floating b) => b -> b -> b -> b -> Colour b
sRGBranged m r' g' b' = rgb709 r g b
 where
  r = invTransferFunction (r'/m)
  g = invTransferFunction (g'/m)
  b = invTransferFunction (b'/m)

sRGB :: (Ord b, Floating b, Integral a, Bounded a) =>
        a -> a -> a -> Colour b
sRGB r' g' b' = sRGBranged (fromIntegral $ maxBound `asTypeOf` r')
                           (fromIntegral r')
                           (fromIntegral g')
                           (fromIntegral b')

sRGB24 :: (Ord b, Floating b) => Word8 -> Word8 -> Word8 -> Colour b
sRGB24 = sRGB

toSRGBranged :: (RealFrac b, Floating b) =>
                b -> Colour b -> (b, b, b)
toSRGBranged m c = (r', g', b')
 where
  (r,g,b) = toRGB709 c
  r' = m*transferFunction r
  g' = m*transferFunction g
  b' = m*transferFunction b

{- Results are clamped and quantized -}
toSRGB :: (RealFrac b, Floating b, Integral a, Bounded a) =>
          Colour b -> (a,a,a)
toSRGB c = (r', g', b')
 where
  (r'0, g'0, b'0) = toSRGBranged (fromIntegral m) c
  (r', g', b') = (quantize r'0, quantize g'0, quantize b'0)
  m = maxBound `asTypeOf` r'

toSRGB24 :: (RealFrac b, Floating b) => Colour b -> (Word8, Word8, Word8)
toSRGB24 = toSRGB

sRGB24shows :: (RealFrac b, Floating b) => Colour b -> ShowS
sRGB24shows c =
  ("#"++) . showHex2 r' . showHex2 g' . showHex2 b'
 where
  (r', g', b') = toSRGB24 c
  showHex2 x | x <= 0xf = ("0"++) . showHex x
             | otherwise = showHex x
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

sRGB24read x | length rx /= 1 || not (null (snd (head rx))) =
  error "Data.Colour.SRGB.readSRGB24: no parse"
             | otherwise = fst (head rx)
 where
  rx = sRGB24reads x

--------------------------------------------------------------------------

sRGB24showlength_prop x = length (sRGB24show x) == 7

readshowSRGB24_prop x =
  sRGB24show (sRGB24read (sRGB24show x)) == sRGB24show x