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
module Data.Colour.Names where

import Prelude hiding (tan)
import Data.Colour.SRGB
import Data.Colour

aliceblue :: (Ord a, Floating a) => Colour a
aliceblue = sRGB24 0xF0 0xF8 0xFF

antiquewhite :: (Ord a, Floating a) => Colour a
antiquewhite = sRGB24 0xFA 0xEB 0xD7

aqua :: (Ord a, Floating a) => Colour a
aqua = sRGB24 0x00 0xFF 0xFF

aquamarine :: (Ord a, Floating a) => Colour a
aquamarine = sRGB24 0x7F 0xFF 0xD4

azure :: (Ord a, Floating a) => Colour a
azure = sRGB24 0xF0 0xFF 0xFF

beige :: (Ord a, Floating a) => Colour a
beige = sRGB24 0xF5 0xF5 0xDC

bisque :: (Ord a, Floating a) => Colour a
bisque = sRGB24 0xFF 0xE4 0xC4

black :: (Ord a, Floating a) => Colour a
black = sRGB24 0x00 0x00 0x00

blanchedalmond :: (Ord a, Floating a) => Colour a
blanchedalmond = sRGB24 0xFF 0xEB 0xCD

blue :: (Ord a, Floating a) => Colour a
blue = sRGB24 0x00 0x00 0xFF

blueviolet :: (Ord a, Floating a) => Colour a
blueviolet = sRGB24 0x8A 0x2B 0xE2

brown :: (Ord a, Floating a) => Colour a
brown = sRGB24 0xA5 0x2A 0x2A

burlywood :: (Ord a, Floating a) => Colour a
burlywood = sRGB24 0xDE 0xB8 0x87

cadetblue :: (Ord a, Floating a) => Colour a
cadetblue = sRGB24 0x5F 0x9E 0xA0

chartreuse :: (Ord a, Floating a) => Colour a
chartreuse = sRGB24 0x7F 0xFF 0x00

chocolate :: (Ord a, Floating a) => Colour a
chocolate = sRGB24 0xD2 0x69 0x1E

coral :: (Ord a, Floating a) => Colour a
coral = sRGB24 0xFF 0x7F 0x50

cornflowerblue :: (Ord a, Floating a) => Colour a
cornflowerblue = sRGB24 0x64 0x95 0xED

cornsilk :: (Ord a, Floating a) => Colour a
cornsilk = sRGB24 0xFF 0xF8 0xDC

crimson :: (Ord a, Floating a) => Colour a
crimson = sRGB24 0xDC 0x14 0x3C

cyan :: (Ord a, Floating a) => Colour a
cyan = sRGB24 0x00 0xFF 0xFF

darkblue :: (Ord a, Floating a) => Colour a
darkblue = sRGB24 0x00 0x00 0x8B

darkcyan :: (Ord a, Floating a) => Colour a
darkcyan = sRGB24 0x00 0x8B 0x8B

darkgoldenrod :: (Ord a, Floating a) => Colour a
darkgoldenrod = sRGB24 0xB8 0x86 0x0B

darkgrey :: (Ord a, Floating a) => Colour a
darkgrey = sRGB24 0xA9 0xA9 0xA9

darkgray :: (Ord a, Floating a) => Colour a
darkgray = darkgrey

darkgreen :: (Ord a, Floating a) => Colour a
darkgreen = sRGB24 0x00 0x64 0x00

darkkhaki :: (Ord a, Floating a) => Colour a
darkkhaki = sRGB24 0xBD 0xB7 0x6B

darkmagenta :: (Ord a, Floating a) => Colour a
darkmagenta = sRGB24 0x8B 0x00 0x8B

darkolivegreen :: (Ord a, Floating a) => Colour a
darkolivegreen = sRGB24 0x55 0x6B 0x2F

darkorange :: (Ord a, Floating a) => Colour a
darkorange = sRGB24 0xFF 0x8C 0x00

darkorchid :: (Ord a, Floating a) => Colour a
darkorchid = sRGB24 0x99 0x32 0xCC

darkred :: (Ord a, Floating a) => Colour a
darkred = sRGB24 0x8B 0x00 0x00

darksalmon :: (Ord a, Floating a) => Colour a
darksalmon = sRGB24 0xE9 0x96 0x7A

darkseagreen :: (Ord a, Floating a) => Colour a
darkseagreen = sRGB24 0x8F 0xBC 0x8F

darkslateblue :: (Ord a, Floating a) => Colour a
darkslateblue = sRGB24 0x48 0x3D 0x8B

darkturqoise :: (Ord a, Floating a) => Colour a
darkturqoise = sRGB24 0x00 0xCE 0xD1

darkslategrey :: (Ord a, Floating a) => Colour a
darkslategrey = sRGB24 0x2F 0x4F 0x4F

darkslategray :: (Ord a, Floating a) => Colour a
darkslategray = darkslategrey

darkviolet :: (Ord a, Floating a) => Colour a
darkviolet = sRGB24 0x94 0x00 0xD3

deeppink :: (Ord a, Floating a) => Colour a
deeppink = sRGB24 0xFF 0x14 0x93

deepskyblue :: (Ord a, Floating a) => Colour a
deepskyblue = sRGB24 0x00 0xBF 0xFF

dimgrey :: (Ord a, Floating a) => Colour a
dimgrey = sRGB24 0x69 0x69 0x69

dimgray :: (Ord a, Floating a) => Colour a
dimgray = dimgrey

dodgerblue :: (Ord a, Floating a) => Colour a
dodgerblue = sRGB24 0x1E 0x90 0xFF

firebrick :: (Ord a, Floating a) => Colour a
firebrick = sRGB24 0xB2 0x22 0x22

floralwhite :: (Ord a, Floating a) => Colour a
floralwhite = sRGB24 0xFF 0xFA 0xF0

forestgreen :: (Ord a, Floating a) => Colour a
forestgreen = sRGB24 0x22 0x8B 0x22

fuchsia :: (Ord a, Floating a) => Colour a
fuchsia = sRGB24 0xFF 0x00 0xFF

gainsboro :: (Ord a, Floating a) => Colour a
gainsboro = sRGB24 0xDC 0xDC 0xDC

ghostwhite :: (Ord a, Floating a) => Colour a
ghostwhite = sRGB24 0xF8 0xF8 0xFF

gold :: (Ord a, Floating a) => Colour a
gold = sRGB24 0xFF 0xD7 0x00

goldenrod :: (Ord a, Floating a) => Colour a
goldenrod = sRGB24 0xDA 0xA5 0x20

grey :: (Ord a, Floating a) => Colour a
grey = sRGB24 0x80 0x80 0x80

gray :: (Ord a, Floating a) => Colour a
gray = grey

green :: (Ord a, Floating a) => Colour a
green = sRGB24 0x00 0x80 0x00

greenyellow :: (Ord a, Floating a) => Colour a
greenyellow = sRGB24 0xAD 0xFF 0x2F

honeydew :: (Ord a, Floating a) => Colour a
honeydew = sRGB24 0xF0 0xFF 0xF0

hotpink :: (Ord a, Floating a) => Colour a
hotpink = sRGB24 0xFF 0x69 0xB4

indianred :: (Ord a, Floating a) => Colour a
indianred = sRGB24 0xCD 0x5C 0x5C

indigo :: (Ord a, Floating a) => Colour a
indigo = sRGB24 0x4B 0x00 0x82

ivory :: (Ord a, Floating a) => Colour a
ivory = sRGB24 0xFF 0xFF 0xF0

khaki :: (Ord a, Floating a) => Colour a
khaki = sRGB24 0xF0 0xE6 0x8C

lavender :: (Ord a, Floating a) => Colour a
lavender = sRGB24 0xE6 0xE6 0xFA

lavenderblush :: (Ord a, Floating a) => Colour a
lavenderblush = sRGB24 0xFF 0xF0 0xF5

lawngreen :: (Ord a, Floating a) => Colour a
lawngreen = sRGB24 0x7C 0xFC 0x00

lemonchiffon :: (Ord a, Floating a) => Colour a
lemonchiffon = sRGB24 0xFF 0xFA 0xCD

lightblue :: (Ord a, Floating a) => Colour a
lightblue = sRGB24 0xAD 0xD8 0xE6

lightcoral :: (Ord a, Floating a) => Colour a
lightcoral = sRGB24 0xF0 0x80 0x80

lightcyan :: (Ord a, Floating a) => Colour a
lightcyan = sRGB24 0xE0 0xFF 0xFF

lightgoldenrodyellow :: (Ord a, Floating a) => Colour a
lightgoldenrodyellow = sRGB24 0xFA 0xFA 0xD2

lightgreen :: (Ord a, Floating a) => Colour a
lightgreen = sRGB24 0x90 0xEE 0x90

lightgrey :: (Ord a, Floating a) => Colour a
lightgrey = sRGB24 0xD3 0xD3 0xD3

lightgray :: (Ord a, Floating a) => Colour a
lightgray = lightgrey

lightpink :: (Ord a, Floating a) => Colour a
lightpink = sRGB24 0xFF 0xB6 0xC1

lightsalmon :: (Ord a, Floating a) => Colour a
lightsalmon = sRGB24 0xFF 0xA0 0x7A

lightseagreen :: (Ord a, Floating a) => Colour a
lightseagreen = sRGB24 0x20 0xB2 0xAA

lightskyblue :: (Ord a, Floating a) => Colour a
lightskyblue = sRGB24 0x87 0xCE 0xFA

lightslategrey :: (Ord a, Floating a) => Colour a
lightslategrey = sRGB24 0x77 0x88 0x99

lightslategray :: (Ord a, Floating a) => Colour a
lightslategray = lightslategrey

lisghtsteelblue :: (Ord a, Floating a) => Colour a
lisghtsteelblue = sRGB24 0xB0 0xC4 0xDE

lightyellow :: (Ord a, Floating a) => Colour a
lightyellow = sRGB24 0xFF 0xFF 0xE0

lime :: (Ord a, Floating a) => Colour a
lime = sRGB24 0x00 0xFF 0x00

limegreen :: (Ord a, Floating a) => Colour a
limegreen = sRGB24 0x32 0xCD 0x32

linen :: (Ord a, Floating a) => Colour a
linen = sRGB24 0xFA 0xF0 0xE6

magenta :: (Ord a, Floating a) => Colour a
magenta = sRGB24 0xFF 0x00 0xFF

maroon :: (Ord a, Floating a) => Colour a
maroon = sRGB24 0x80 0x00 0x00

mediumaquamarine :: (Ord a, Floating a) => Colour a
mediumaquamarine = sRGB24 0x66 0xCD 0xAA

mediumblue :: (Ord a, Floating a) => Colour a
mediumblue = sRGB24 0x00 0x00 0xCD

mediumorchid :: (Ord a, Floating a) => Colour a
mediumorchid = sRGB24 0xBA 0x55 0xD3

mediumpurple :: (Ord a, Floating a) => Colour a
mediumpurple = sRGB24 0x93 0x70 0xDB

mediumseagreen :: (Ord a, Floating a) => Colour a
mediumseagreen = sRGB24 0x3C 0xB3 0x71

mediumslateblue :: (Ord a, Floating a) => Colour a
mediumslateblue = sRGB24 0x7B 0x68 0xEE

mediumspringgreen :: (Ord a, Floating a) => Colour a
mediumspringgreen = sRGB24 0x00 0xFA 0x9A

mediumturquoise :: (Ord a, Floating a) => Colour a
mediumturquoise = sRGB24 0x48 0xD1 0xCC

mediumvioletred :: (Ord a, Floating a) => Colour a
mediumvioletred = sRGB24 0xC7 0x15 0x85

midnightblue :: (Ord a, Floating a) => Colour a
midnightblue = sRGB24 0x19 0x19 0x70

mintcream :: (Ord a, Floating a) => Colour a
mintcream = sRGB24 0xF5 0xFF 0xFA

mistyrose :: (Ord a, Floating a) => Colour a
mistyrose = sRGB24 0xFF 0xE4 0xE1

moccasin :: (Ord a, Floating a) => Colour a
moccasin = sRGB24 0xFF 0xE4 0xB5

navajowhite :: (Ord a, Floating a) => Colour a
navajowhite = sRGB24 0xFF 0xDE 0xAD

navy :: (Ord a, Floating a) => Colour a
navy = sRGB24 0x00 0x00 0x80

navyblue :: (Ord a, Floating a) => Colour a
navyblue = sRGB24 0x9F 0xAF 0xDF

oldlace :: (Ord a, Floating a) => Colour a
oldlace = sRGB24 0xFD 0xF5 0xE6

olive :: (Ord a, Floating a) => Colour a
olive = sRGB24 0x80 0x80 0x00

olivedrab :: (Ord a, Floating a) => Colour a
olivedrab = sRGB24 0x6B 0x8E 0x23

orange :: (Ord a, Floating a) => Colour a
orange = sRGB24 0xFF 0xA5 0x00

orangered :: (Ord a, Floating a) => Colour a
orangered = sRGB24 0xFF 0x45 0x00

orchid :: (Ord a, Floating a) => Colour a
orchid = sRGB24 0xDA 0x70 0xD6

palegoldenrod :: (Ord a, Floating a) => Colour a
palegoldenrod = sRGB24 0xEE 0xE8 0xAA

palegreen :: (Ord a, Floating a) => Colour a
palegreen = sRGB24 0x98 0xFB 0x98

paleturquoise :: (Ord a, Floating a) => Colour a
paleturquoise = sRGB24 0xAF 0xEE 0xEE

palevioletred :: (Ord a, Floating a) => Colour a
palevioletred = sRGB24 0xDB 0x70 0x93

papayawhip :: (Ord a, Floating a) => Colour a
papayawhip = sRGB24 0xFF 0xEF 0xD5

peachpuff :: (Ord a, Floating a) => Colour a
peachpuff = sRGB24 0xFF 0xDA 0xB9

peru :: (Ord a, Floating a) => Colour a
peru = sRGB24 0xCD 0x85 0x3F

pink :: (Ord a, Floating a) => Colour a
pink = sRGB24 0xFF 0xC0 0xCB

plum :: (Ord a, Floating a) => Colour a
plum = sRGB24 0xDD 0xA0 0xDD

powderblue :: (Ord a, Floating a) => Colour a
powderblue = sRGB24 0xB0 0xE0 0xE6

purple :: (Ord a, Floating a) => Colour a
purple = sRGB24 0x80 0x00 0x80

red :: (Ord a, Floating a) => Colour a
red = sRGB24 0xFF 0x00 0x00

rosybrown :: (Ord a, Floating a) => Colour a
rosybrown = sRGB24 0xBC 0x8F 0x8F

royalblue :: (Ord a, Floating a) => Colour a
royalblue = sRGB24 0x41 0x69 0xE1

saddlebrown :: (Ord a, Floating a) => Colour a
saddlebrown = sRGB24 0x8B 0x45 0x13

salmon :: (Ord a, Floating a) => Colour a
salmon = sRGB24 0xFA 0x80 0x72

sandybrown :: (Ord a, Floating a) => Colour a
sandybrown = sRGB24 0xF4 0xA4 0x60

seagreen :: (Ord a, Floating a) => Colour a
seagreen = sRGB24 0x2E 0x8B 0x57

seashell :: (Ord a, Floating a) => Colour a
seashell = sRGB24 0xFF 0xF5 0xEE

sienna :: (Ord a, Floating a) => Colour a
sienna = sRGB24 0xA0 0x52 0x2D

silver :: (Ord a, Floating a) => Colour a
silver = sRGB24 0xC0 0xC0 0xC0

skyblue :: (Ord a, Floating a) => Colour a
skyblue = sRGB24 0x87 0xCE 0xEB

slateblue :: (Ord a, Floating a) => Colour a
slateblue = sRGB24 0x6A 0x5A 0xCD

snow :: (Ord a, Floating a) => Colour a
snow = sRGB24 0xFF 0xFA 0xFA

springgreen :: (Ord a, Floating a) => Colour a
springgreen = sRGB24 0x00 0xFF 0x7F

steelblue :: (Ord a, Floating a) => Colour a
steelblue = sRGB24 0x46 0x82 0xB4

tan :: (Ord a, Floating a) => Colour a
tan = sRGB24 0xD2 0xB4 0x8C

teal :: (Ord a, Floating a) => Colour a
teal = sRGB24 0x00 0x80 0x80

thistle :: (Ord a, Floating a) => Colour a
thistle = sRGB24 0xD8 0xBF 0xD8

tomato :: (Ord a, Floating a) => Colour a
tomato = sRGB24 0xFF 0x63 0x47

turquoise :: (Ord a, Floating a) => Colour a
turquoise = sRGB24 0x40 0xE0 0xD0

violet :: (Ord a, Floating a) => Colour a
violet = sRGB24 0xEE 0x82 0xEE

wheat :: (Ord a, Floating a) => Colour a
wheat = sRGB24 0xF5 0xDE 0xB3

white :: (Ord a, Floating a) => Colour a
white = sRGB24 0xFF 0xFF 0xFF

whitesmoke :: (Ord a, Floating a) => Colour a
whitesmoke = sRGB24 0xF5 0xF5 0xF5

yellow :: (Ord a, Floating a) => Colour a
yellow = sRGB24 0xFF 0xFF 0x00

yellowgreen :: (Ord a, Floating a) => Colour a
yellowgreen = sRGB24 0x9A 0xCD 0x32

