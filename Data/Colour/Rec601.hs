module Data.Colour.Rec601
 (luma
 ,y'PbPr, toY'PbPr
 ,y'CbCr, toY'CbCr
 )
where

import Data.Colour.SRGB
import Data.Colour
import Data.Word

{- rec 601 luma -}
luma :: (Floating a, RealFrac a) => Colour a -> a
luma c = transformBy [0.299, 0.587, 0.114]
 where
  (r',g',b') = toSRGBranged 1.0 c
  transformBy l = sum $ zipWith (*) l [r',g',b']

y'PbPr :: (Floating a, RealFrac a)  => a -> a -> a -> Colour a
y'PbPr y' pb pr = sRGBranged 1.0 r' g' b'
 where
  r' = y' + (0.701/0.5)*pr
  g' = (y' - 0.299*r' - 0.114*b')/0.587
  b' = y' + (0.886/0.5)*pb

toY'PbPr :: (Floating a, RealFrac a)  => Colour a -> (a, a, a)
toY'PbPr c = (y', pb, pr)
 where
  y' = luma c
  (r', g', b') = toSRGBranged 1.0 c
  pb = (0.5/0.886)*(b' - y')
  pr = (0.5/0.701)*(r' - y')

y'CbCr :: (Floating a, RealFrac a)  => Word8 -> Word8 -> Word8 -> Colour a
y'CbCr y' cb cr = y'PbPr y'0 pb pr
 where
  y'0 = ((fromIntegral y') - 16)/219
  pb  = ((fromIntegral cb) - 128)/224
  pr  = ((fromIntegral cr) - 128)/224

toY'CbCr :: (Floating a, RealFrac a)  => Colour a -> (Word8, Word8, Word8)
toY'CbCr c = (quantize $ 16 + 219*y'
             ,quantize $ 128 + 224*pb
             ,quantize $ 128 + 224*pr)
 where
  (y', pb, pr) = toY'PbPr c
