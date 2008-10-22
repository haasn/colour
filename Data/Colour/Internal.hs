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
import Data.Monoid

data Red = Red
data Green = Green
data Blue = Blue

-- |This type represents the human preception of colour.
-- The @a@ parameter is a numeric type used internally for the representation.

-- Internally we store the colour in linear ITU-R BT.709 RGB colour space. 
data Colour a = RGB !(Chan Red a) !(Chan Green a) !(Chan Blue a) deriving (Eq)

-- |Constructs a 'Colour' from RGB values using the /linear/ RGB colour
-- space specified in Rec.709.
rgb709 :: a -> a -> a -> Colour a
rgb709 r g b = RGB (Chan r) (Chan g) (Chan b)

-- |Return RGB values using the /linear/ RGB colour space specified in
-- Rec.709.
toRGB709 :: Colour a -> (a,a,a)
toRGB709 (RGB (Chan r) (Chan g) (Chan b)) = (r,g,b)

-- |Change the type used to represent the colour coordinates.
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

-- |This type represents a 'Colour' that may be semi-transparent.

-- Internally we use a premultiplied-alpha representation.
data AlphaColour a = RGBA !(Colour a) !(Chan Alpha a) deriving (Eq)

instance (Fractional a) => Show (AlphaColour a) where
  showsPrec _ ac | a == 0 = ("transparent"++)
                 | otherwise = shows c . (" `withOpacity` "++) . shows a
   where
    a = alphaChannel ac
    c = colourChannel ac

-- |This 'AlphaColour' is entirely transparent and has no associated
-- 'colourChannel'.
transparent :: (Num a) => AlphaColour a
transparent = RGBA (RGB Chan.empty Chan.empty Chan.empty) Chan.empty

-- |Change the type used to represent the colour coordinates.
alphaColourConvert :: (Fractional b, Real a) =>
  AlphaColour a -> AlphaColour b
alphaColourConvert (RGBA c a) = RGBA (colourConvert c) (Chan.convert a)

-- |Creates an opaque 'AlphaColour' from a 'Colour'.
opaque :: (Num a) => Colour a -> AlphaColour a
opaque c = RGBA c Chan.full

-- |Returns a 'AlphaColour' more transparent by a factor of @o@.
fade :: (Num a) => a -> AlphaColour a -> AlphaColour a
fade o (RGBA c a) = RGBA (scale o c) (Chan.scale o a)

-- |Creates an 'AlphaColour' from a 'Colour' with a given opacity.
--
-- >c `withOpacity` o == fade o (opaque c) 
withOpacity :: (Num a) => Colour a -> a -> AlphaColour a
c `withOpacity` o = RGBA (scale o c) (Chan o)

--------------------------------------------------------------------------
-- Blending
--------------------------------------------------------------------------

class AffineSpace f where
 -- |Compute a affine Combination (weighted-average) of points.
 -- The last parameter will get the remaining weight.
 -- e.g.
 --
 -- >affineCombo [(0.2,a), (0.3,b)] c == 0.2*a + 0.3*b + 0.4*c
 affineCombo :: (Num a) => [(a,f a)] -> f a -> f a

-- |Compute the weighted average of two points.
-- e.g.
--
-- >blend 0.4 a b = 0.4*a + 0.6*b
blend :: (Num a, AffineSpace f) => a -> f a -> f a -> f a
blend weight c1 c2 = affineCombo [(weight,c1)] c2

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

--------------------------------------------------------------------------
-- composite
--------------------------------------------------------------------------

class Composite f where
 -- |@c1 \`over\` c2@ returns the 'Colour' created by compositing the
 -- 'AlphaColour' @c1@ over @c2@, which may be either a 'Colour' or
 -- 'AlphaColour'.
 over :: (Num a) => AlphaColour a -> f a -> f a

instance Composite Colour where
 (RGBA (RGB r0 g0 b0) (Chan a0)) `over` (RGB r1 g1 b1) =
   RGB (Chan.over r0 a0 r1)
       (Chan.over g0 a0 g1)
       (Chan.over b0 a0 b1)

instance Composite AlphaColour where
 c0@(RGBA _ a0@(Chan a0')) `over` (RGBA c1 a1) =
   RGBA (c0 `over` c1) (Chan.over a0 a0' a1)

-- |Composites @c1@ over @c2@ using opacity @a@.
compositeWith :: (Num a) => a -> Colour a -> Colour a -> Colour a
compositeWith a c1 c2 = (c1 `withOpacity` a) `over` c2

-- | 'AlphaColour' forms a monoid with 'over' and 'transparent'.
instance (Num a) => Monoid (AlphaColour a) where
  mempty = transparent
  mappend = over

-- |'round's and then clamps @x@ between 0 and 'maxBound'.
quantize :: (RealFrac a1, Integral a, Bounded a) => a1 -> a
quantize x | x <= fromIntegral l = l
           | fromIntegral h <= x = h
           | otherwise           = round x
 where
  l = minBound
  h = maxBound

{- Avoid using -}
-- |Returns the opacity of an 'AlphaColour'.
-- This function is provided only for converting to other datatypes.
-- Its use is discouraged.
-- Instead compose the 'AlphaColour' with another 'Colour' and extract
-- the resulting 'Colour' components.
alphaChannel :: AlphaColour a -> a
alphaChannel (RGBA _ (Chan a)) = a

-- |Returns the colour of an 'AlphaColour'.
-- @colourChannel transparent@ is undefined and may result in @nan@ or an
-- error.
-- This function is provided only for converting to other datatypes.
-- Its use is discouraged.
-- Instead compose the 'AlphaColour' with another 'Colour' and extract the
-- resulting 'Colour' components.
colourChannel :: (Fractional a) => AlphaColour a -> Colour a
colourChannel (RGBA (RGB r g b) (Chan a)) =
  RGB (Chan.scale a' r)
      (Chan.scale a' g)
      (Chan.scale a' b)

 where
  a' = recip a

--------------------------------------------------------------------------
-- not for export
--------------------------------------------------------------------------

scale s (RGB r g b) = RGB (Chan.scale s r)
                          (Chan.scale s g)
                          (Chan.scale s b)

rgbAdd (RGB r1 g1 b1) (RGB r2 g2 b2) =
  RGB (r1 `Chan.add` r2) (g1 `Chan.add` g2) (b1 `Chan.add` b2)

rgbaAdd (RGBA c1 a1) (RGBA c2 a2) =
  RGBA (c1 `rgbAdd` c2) (a1 `Chan.add` a2)