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
-- - "Data.Colour.SRGB.Linear"
--
-- - "Data.Colour.CIE"
--
-- Colours can be specified in a generic 'Data.Colour.RGBSpace.RGBSpace'
-- by using
--
-- - "Data.Colour.RGBSpace"


--TODO
-- - "Data.Colour.HDTV"
--
-- - "Data.Colour.SDTV"
module Data.Colour
 ( -- *Colour type
  Colour
 ,colourConvert

 ,AlphaColour
 ,opaque, withOpacity
 ,transparent
 ,alphaColourConvert
 ,alphaChannel

 -- *Colour operations
 -- |These operations allow combine and modify existing colours
 ,AffineSpace(..), blend

 ,ColourOps(..)
 ,dissolve, atop
 )
where

import Data.Char
import Data.Colour.Internal
import qualified Data.Colour.SRGB.Linear
import Data.Colour.CIE.Chromaticity (app_prec, infix_prec)

instance (Fractional a) => Show (Colour a) where
  showsPrec d c = showParen (d > app_prec) showStr
   where
    showStr = showString linearConstructorQualifiedName
            . showString " " . (showsPrec (app_prec+1) r)
            . showString " " . (showsPrec (app_prec+1) g)
            . showString " " . (showsPrec (app_prec+1) b)
    Data.Colour.SRGB.Linear.RGB r g b = Data.Colour.SRGB.Linear.toRGB c

instance (Fractional a, Read a) => Read (Colour a) where
  readsPrec d r = readParen (d > app_prec)
                  (\r -> [(Data.Colour.SRGB.Linear.rgb r0 g0 b0,t)
                         |(name,s) <- mylex r
                         ,name `elem` [linearConstructorName
                                      ,linearConstructorQualifiedName]
                         ,(r0,s0) <- readsPrec (app_prec+1) s
                         ,(g0,s1) <- readsPrec (app_prec+1) s0
                         ,(b0,t)  <- readsPrec (app_prec+1) s1]) r
   where
    mylex = return 
          . span (\c -> isAlphaNum c || c `elem` "._'")
          . dropWhile isSpace

linearConstructorQualifiedName = "Data.Colour.SRGB.Linear.rgb"
linearConstructorName = "rgb"

instance (Fractional a) => Show (AlphaColour a) where
  showsPrec d ac | a == 0 = showString "transparent"
                 | otherwise = showParen (d > infix_prec) showStr
   where
    showStr = showsPrec (infix_prec+1) c
            . showString " `withOpacity` "
            . showsPrec (infix_prec+1) a
    a = alphaChannel ac
    c = colourChannel ac

instance (Fractional a, Read a) => Read (AlphaColour a) where
  readsPrec d r = [(transparent,s)|("transparent",s) <- lex r]
               ++ readParen (d > infix_prec)
                  (\r -> [(c `withOpacity` o,s)
                         |(c,r0) <- readsPrec (infix_prec+1) r
                         ,("`",r1) <- lex r0
                         ,("withOpacity",r2) <- lex r1
                         ,("`",r3) <- lex r2
                         ,(o,s)  <- readsPrec (infix_prec+1) r3]) r
