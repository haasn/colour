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
-- |Datatypes for representing the human perception of colour.
-- Includes common operations for blending and compositing colours.
-- The most common way of creating colours is either by name
-- (see "Data.Colour.Names") or by giving an sRGB triple 
-- (see "Data.Colour.SRGB").
--
-- Methods of specifying Colours can be found in 
--
-- - "Data.Colour.SRGB"
--
-- - "Data.Colour.CIE"
--
-- - "Data.Colour.HDTV"
--
-- - "Data.Colour.SDTV"
module Data.Colour
 (Colour
 ,colourConvert

 ,AlphaColour
 ,alphaColour, fade, withOpacity
 ,transparent
 ,alphaColourConvert
 ,alphaChannel, colourChannel

 ,AffineSpace(..), blend

 ,Composite(..), compositeWith
 )
where

import Data.Colour.Internal