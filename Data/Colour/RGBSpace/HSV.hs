{-
Copyright (c) 2008,2009
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

module Data.Colour.RGBSpace.HSV 
 (RGB
 ,hsv
 ,hue, saturation, value
 ,fromHSV
 )
where

import Data.Colour.RGB

hsv :: (Fractional a, Ord a) => RGB a -> (a,a,a)
hsv rgb = (h,s,v)
 where
  (h,_,_,s,v) = hslsv rgb

saturation :: (Fractional a, Ord a) => RGB a -> a
saturation rgb = s
 where
  (_,_,_,s,_) = hslsv rgb

value :: (Fractional a, Ord a) => RGB a -> a
value rgb = v
 where
  (_,_,_,_,v) = hslsv rgb

fromHSV :: (RealFrac a, Ord a) => (a,a,a) -> RGB a
fromHSV (h,s,v) = case hi of
    0 -> RGB v t p
    1 -> RGB q v p
    2 -> RGB p v t
    3 -> RGB p q v
    4 -> RGB t p v
    5 -> RGB v p q
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)
