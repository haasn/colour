module Data.Colour
 (Colour
 ,rgb709, toRGB709
 ,colourConvert

 ,AlphaColour
 ,alphaColour, fade, withOpacity
 ,transparent
 ,alphaColourConvert

 ,AffineSpace(..), blend

 ,compositeWith, Composite(..)

 ,quantize
 )
where

import Data.List
import qualified Data.Colour.Chan as Chan
import Data.Colour.Chan (Chan(Chan))

{- Internal representation is
   Rec. 709 Primaries and D65 white point -}
data Red = Red
data Green = Green
data Blue = Blue
data Colour a = RGB !(Chan Red a) !(Chan Green a) !(Chan Blue a) deriving (Eq)

rgb709 :: a -> a -> a -> Colour a
rgb709 r g b = RGB (Chan r) (Chan g) (Chan b)

toRGB709 :: Colour a -> (a,a,a)
toRGB709 (RGB (Chan r) (Chan g) (Chan b)) = (r,g,b)

colourConvert :: (Fractional b, Real a) => Colour a -> Colour b
colourConvert (RGB r g b) =
  RGB (Chan.convert r) (Chan.convert g) (Chan.convert b)

data Alpha = Alpha
data AlphaColour a = RGBA !(Colour a) !(Chan Alpha a) -- premultiplied alpha

transparent :: (Num a) => AlphaColour a
transparent = RGBA (RGB Chan.empty Chan.empty Chan.empty) Chan.empty

alphaColourConvert :: (Fractional b, Real a) =>
  AlphaColour a -> AlphaColour b
alphaColourConvert (RGBA c a) = RGBA (colourConvert c) (Chan.convert a)

alphaColour :: (Num a) => Colour a -> AlphaColour a
alphaColour c = RGBA c Chan.full

fade :: (Num a) => a -> AlphaColour a -> AlphaColour a
fade o (RGBA c a) = RGBA (scale o c) (Chan.scale o a)

{- c `withOpacity` o == fade o (toRGBA c) -}
withOpacity :: (Num a) => Colour a -> a -> AlphaColour a
c `withOpacity` o = RGBA (scale o c) (Chan o)

{- blend -}

class AffineSpace f where
 affineCombo :: (Num a) => [(a,f a)] -> f a -> f a

blend weight c1 c2 = affineCombo [(weight,c2)] c1

instance AffineSpace Colour where
 affineCombo l z =
   foldl1' rgbAdd [scale w a | (w,a) <- (1-total,z):l]
  where
   total = sum $ map fst l

instance AffineSpace AlphaColour where
 affineCombo l z =
   foldl1' rgbaAdd [fade w a | (w,a) <- (1-total,z):l]
  where
   total = sum $ map fst l

{- compose -}

compositeWith :: (Num a) => a -> Colour a -> Colour a -> Colour a
compositeWith a (RGB r0 g0 b0) (RGB r1 g1 b1) =
  RGB (Chan.over r0 a r1)
      (Chan.over g0 a g1)
      (Chan.over b0 a b1)

class Composite f where
 over :: (Num a) => AlphaColour a -> f a -> f a

instance Composite Colour where
 (RGBA (RGB r0 g0 b0) (Chan a0)) `over` (RGB r1 g1 b1) =
   RGB (Chan.over r0 a0 r1)
       (Chan.over g0 a0 g1)
       (Chan.over b0 a0 b1)

instance Composite AlphaColour where
 c0@(RGBA _ a0@(Chan a0')) `over` (RGBA c1 a1) =
   RGBA (c0 `over` c1) (Chan.over a0 a0' a1)

quantize x | x <= fromIntegral l = l
           | fromIntegral h <= x = h
           | otherwise           = round x
 where
  l = minBound
  h = maxBound

{- not for export -}
scale s (RGB r g b) = RGB (Chan.scale s r)
                          (Chan.scale s g)
                          (Chan.scale s b)

rgbAdd (RGB r1 g1 b1) (RGB r2 g2 b2) =
  RGB (r1 `Chan.add` r2) (g1 `Chan.add` g2) (b1 `Chan.add` b2)

rgbaAdd (RGBA c1 a1) (RGBA c2 a2) =
  RGBA (c1 `rgbAdd` c2) (a1 `Chan.add` a2)