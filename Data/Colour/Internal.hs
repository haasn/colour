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
module Data.Colour.Internal where

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

instance (Show a) => Show (Colour a) where
  showsPrec _ c = ("(rgb709 "++) . (shows r) . (" "++)
                                 . (shows g) . (" "++)
                                 . (shows b) . (")"++)
   where
    (r,g,b) = toRGB709 c

data Alpha = Alpha
-- premultiplied alpha
data AlphaColour a = RGBA !(Colour a) !(Chan Alpha a) deriving (Eq)

instance (Fractional a) => Show (AlphaColour a) where
  showsPrec _ ac | a == 0 = ("transparent"++)
                 | otherwise = shows c . (" `withOpacity` "++) . shows a
   where
    a = alphaChannel ac
    c = colourChannel ac

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

blend :: (Num a, AffineSpace f) => a -> f a -> f a -> f a
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

quantize :: (RealFrac a1, Integral a, Bounded a) => a1 -> a
quantize x | x <= fromIntegral l = l
           | fromIntegral h <= x = h
           | otherwise           = round x
 where
  l = minBound
  h = maxBound

{- Avoid using -}
alphaChannel :: AlphaColour a -> a
alphaChannel (RGBA _ (Chan a)) = a

colourChannel :: (Fractional a) => AlphaColour a -> Colour a
colourChannel (RGBA (RGB r g b) (Chan a)) | a == 0 =
  error "Data.Colour: transparent has no colour channel"
                                          | otherwise =
  RGB (Chan.scale a' r)
      (Chan.scale a' g)
      (Chan.scale a' b)

 where
  a' = recip a

{- not for export -}
scale s (RGB r g b) = RGB (Chan.scale s r)
                          (Chan.scale s g)
                          (Chan.scale s b)

rgbAdd (RGB r1 g1 b1) (RGB r2 g2 b2) =
  RGB (r1 `Chan.add` r2) (g1 `Chan.add` g2) (b1 `Chan.add` b2)

rgbaAdd (RGBA c1 a1) (RGBA c2 a2) =
  RGBA (c1 `rgbAdd` c2) (a1 `Chan.add` a2)