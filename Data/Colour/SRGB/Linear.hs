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
-- |Provides a /linear/ colour space with the same gamut as
-- "Data.Colour.SRGB".
module Data.Colour.SRGB.Linear 
 (rgb, toRGB
 ,rgbSpace
 )
where

import Data.Colour.Internal (Colour(RGB))
import Data.Colour.Chan
import qualified Data.Colour.RGB (RGB(RGB))
import Data.Colour.CIE.Chromaticity
import Data.Colour.CIE.Illuminant
import Data.Colour.RGB (RGBSpace(..))

-- |Constructs a 'Colour' from RGB values using the /linear/ RGB colour
-- with the same gamut as sRGB.
rgb :: Fractional a => a -> a -> a -> Colour a
rgb r g b = RGB (Chan r) (Chan g) (Chan b)

-- |Return RGB values using the /linear/ RGB colour with the same gamut
-- as sRGB.
toRGB :: Fractional a => Colour a -> Data.Colour.RGB.RGB a
toRGB (RGB (Chan r) (Chan g) (Chan b)) = Data.Colour.RGB.RGB r g b

rgbSpace :: Fractional a => RGBSpace a
rgbSpace = RGBSpace (Data.Colour.RGB.RGB
                    (cieChroma 0.64 0.33)
                    (cieChroma 0.30 0.60)
                    (cieChroma 0.15 0.06))
                    d65
