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

-- |Names for colours.
-- Names taken from SVG 1.0 specification.
module Data.Colour.Names where

import Prelude hiding (tan)
import Data.Colour.SRGB
import Data.Colour

aliceblue :: (Ord a, Floating a) => Colour a
aliceblue = sRGB24 240 248 255

antiquewhite :: (Ord a, Floating a) => Colour a
antiquewhite = sRGB24 250 235 215

aqua :: (Ord a, Floating a) => Colour a
aqua = sRGB24 0 255 255

aquamarine :: (Ord a, Floating a) => Colour a
aquamarine = sRGB24 127 255 212

azure :: (Ord a, Floating a) => Colour a
azure = sRGB24 240 255 255

beige :: (Ord a, Floating a) => Colour a
beige = sRGB24 245 245 220

bisque :: (Ord a, Floating a) => Colour a
bisque = sRGB24 255 228 196

-- black is defined in Data.Colour

blanchedalmond :: (Ord a, Floating a) => Colour a
blanchedalmond = sRGB24 255 235 205

blue :: (Ord a, Floating a) => Colour a
blue = sRGB24 0 0 255

blueviolet :: (Ord a, Floating a) => Colour a
blueviolet = sRGB24 138 43 226

brown :: (Ord a, Floating a) => Colour a
brown = sRGB24 165 42 42

burlywood :: (Ord a, Floating a) => Colour a
burlywood = sRGB24 222 184 135

cadetblue :: (Ord a, Floating a) => Colour a
cadetblue = sRGB24 95 158 160

chartreuse :: (Ord a, Floating a) => Colour a
chartreuse = sRGB24 127 255 0

chocolate :: (Ord a, Floating a) => Colour a
chocolate = sRGB24 210 105 30

coral :: (Ord a, Floating a) => Colour a
coral = sRGB24 255 127 80

cornflowerblue :: (Ord a, Floating a) => Colour a
cornflowerblue = sRGB24 100 149 237

cornsilk :: (Ord a, Floating a) => Colour a
cornsilk = sRGB24 255 248 220

crimson :: (Ord a, Floating a) => Colour a
crimson = sRGB24 220 20 60

cyan :: (Ord a, Floating a) => Colour a
cyan = sRGB24 0 255 255

darkblue :: (Ord a, Floating a) => Colour a
darkblue = sRGB24 0 0 139

darkcyan :: (Ord a, Floating a) => Colour a
darkcyan = sRGB24 0 139 139

darkgoldenrod :: (Ord a, Floating a) => Colour a
darkgoldenrod = sRGB24 184 134 11

darkgray :: (Ord a, Floating a) => Colour a
darkgray = sRGB24 169 169 169

darkgreen :: (Ord a, Floating a) => Colour a
darkgreen = sRGB24 0 100 0

darkgrey :: (Ord a, Floating a) => Colour a
darkgrey = sRGB24 169 169 169

darkkhaki :: (Ord a, Floating a) => Colour a
darkkhaki = sRGB24 189 183 107

darkmagenta :: (Ord a, Floating a) => Colour a
darkmagenta = sRGB24 139 0 139

darkolivegreen :: (Ord a, Floating a) => Colour a
darkolivegreen = sRGB24 85 107 47

darkorange :: (Ord a, Floating a) => Colour a
darkorange = sRGB24 255 140 0

darkorchid :: (Ord a, Floating a) => Colour a
darkorchid = sRGB24 153 50 204

darkred :: (Ord a, Floating a) => Colour a
darkred = sRGB24 139 0 0

darksalmon :: (Ord a, Floating a) => Colour a
darksalmon = sRGB24 233 150 122

darkseagreen :: (Ord a, Floating a) => Colour a
darkseagreen = sRGB24 143 188 143

darkslateblue :: (Ord a, Floating a) => Colour a
darkslateblue = sRGB24 72 61 139

darkslategray :: (Ord a, Floating a) => Colour a
darkslategray = sRGB24 47 79 79

darkslategrey :: (Ord a, Floating a) => Colour a
darkslategrey = sRGB24 47 79 79

darkturquoise :: (Ord a, Floating a) => Colour a
darkturquoise = sRGB24 0 206 209

darkviolet :: (Ord a, Floating a) => Colour a
darkviolet = sRGB24 148 0 211

deeppink :: (Ord a, Floating a) => Colour a
deeppink = sRGB24 255 20 147

deepskyblue :: (Ord a, Floating a) => Colour a
deepskyblue = sRGB24 0 191 255

dimgray :: (Ord a, Floating a) => Colour a
dimgray = sRGB24 105 105 105

dimgrey :: (Ord a, Floating a) => Colour a
dimgrey = sRGB24 105 105 105

dodgerblue :: (Ord a, Floating a) => Colour a
dodgerblue = sRGB24 30 144 255

firebrick :: (Ord a, Floating a) => Colour a
firebrick = sRGB24 178 34 34

floralwhite :: (Ord a, Floating a) => Colour a
floralwhite = sRGB24 255 250 240

forestgreen :: (Ord a, Floating a) => Colour a
forestgreen = sRGB24 34 139 34

fuchsia :: (Ord a, Floating a) => Colour a
fuchsia = sRGB24 255 0 255

gainsboro :: (Ord a, Floating a) => Colour a
gainsboro = sRGB24 220 220 220

ghostwhite :: (Ord a, Floating a) => Colour a
ghostwhite = sRGB24 248 248 255

gold :: (Ord a, Floating a) => Colour a
gold = sRGB24 255 215 0

goldenrod :: (Ord a, Floating a) => Colour a
goldenrod = sRGB24 218 165 32

gray :: (Ord a, Floating a) => Colour a
gray = sRGB24 128 128 128

grey :: (Ord a, Floating a) => Colour a
grey = sRGB24 128 128 128

green :: (Ord a, Floating a) => Colour a
green = sRGB24 0 128 0

greenyellow :: (Ord a, Floating a) => Colour a
greenyellow = sRGB24 173 255 47

honeydew :: (Ord a, Floating a) => Colour a
honeydew = sRGB24 240 255 240

hotpink :: (Ord a, Floating a) => Colour a
hotpink = sRGB24 255 105 180

indianred :: (Ord a, Floating a) => Colour a
indianred = sRGB24 205 92 92

indigo :: (Ord a, Floating a) => Colour a
indigo = sRGB24 75 0 130

ivory :: (Ord a, Floating a) => Colour a
ivory = sRGB24 255 255 240

khaki :: (Ord a, Floating a) => Colour a
khaki = sRGB24 240 230 140

lavender :: (Ord a, Floating a) => Colour a
lavender = sRGB24 230 230 250

lavenderblush :: (Ord a, Floating a) => Colour a
lavenderblush = sRGB24 255 240 245

lawngreen :: (Ord a, Floating a) => Colour a
lawngreen = sRGB24 124 252 0

lemonchiffon :: (Ord a, Floating a) => Colour a
lemonchiffon = sRGB24 255 250 205

lightblue :: (Ord a, Floating a) => Colour a
lightblue = sRGB24 173 216 230

lightcoral :: (Ord a, Floating a) => Colour a
lightcoral = sRGB24 240 128 128

lightcyan :: (Ord a, Floating a) => Colour a
lightcyan = sRGB24 224 255 255

lightgoldenrodyellow :: (Ord a, Floating a) => Colour a
lightgoldenrodyellow = sRGB24 250 250 210

lightgray :: (Ord a, Floating a) => Colour a
lightgray = sRGB24 211 211 211

lightgreen :: (Ord a, Floating a) => Colour a
lightgreen = sRGB24 144 238 144

lightgrey :: (Ord a, Floating a) => Colour a
lightgrey = sRGB24 211 211 211

lightpink :: (Ord a, Floating a) => Colour a
lightpink = sRGB24 255 182 193

lightsalmon :: (Ord a, Floating a) => Colour a
lightsalmon = sRGB24 255 160 122

lightseagreen :: (Ord a, Floating a) => Colour a
lightseagreen = sRGB24 32 178 170

lightskyblue :: (Ord a, Floating a) => Colour a
lightskyblue = sRGB24 135 206 250

lightslategray :: (Ord a, Floating a) => Colour a
lightslategray = sRGB24 119 136 153

lightslategrey :: (Ord a, Floating a) => Colour a
lightslategrey = sRGB24 119 136 153

lightsteelblue :: (Ord a, Floating a) => Colour a
lightsteelblue = sRGB24 176 196 222

lightyellow :: (Ord a, Floating a) => Colour a
lightyellow = sRGB24 255 255 224

lime :: (Ord a, Floating a) => Colour a
lime = sRGB24 0 255 0

limegreen :: (Ord a, Floating a) => Colour a
limegreen = sRGB24 50 205 50

linen :: (Ord a, Floating a) => Colour a
linen = sRGB24 250 240 230

magenta :: (Ord a, Floating a) => Colour a
magenta = sRGB24 255 0 255

maroon :: (Ord a, Floating a) => Colour a
maroon = sRGB24 128 0 0

mediumaquamarine :: (Ord a, Floating a) => Colour a
mediumaquamarine = sRGB24 102 205 170

mediumblue :: (Ord a, Floating a) => Colour a
mediumblue = sRGB24 0 0 205

mediumorchid :: (Ord a, Floating a) => Colour a
mediumorchid = sRGB24 186 85 211

mediumpurple :: (Ord a, Floating a) => Colour a
mediumpurple = sRGB24 147 112 219

mediumseagreen :: (Ord a, Floating a) => Colour a
mediumseagreen = sRGB24 60 179 113

mediumslateblue :: (Ord a, Floating a) => Colour a
mediumslateblue = sRGB24 123 104 238

mediumspringgreen :: (Ord a, Floating a) => Colour a
mediumspringgreen = sRGB24 0 250 154

mediumturquoise :: (Ord a, Floating a) => Colour a
mediumturquoise = sRGB24 72 209 204

mediumvioletred :: (Ord a, Floating a) => Colour a
mediumvioletred = sRGB24 199 21 133

midnightblue :: (Ord a, Floating a) => Colour a
midnightblue = sRGB24 25 25 112

mintcream :: (Ord a, Floating a) => Colour a
mintcream = sRGB24 245 255 250

mistyrose :: (Ord a, Floating a) => Colour a
mistyrose = sRGB24 255 228 225

moccasin :: (Ord a, Floating a) => Colour a
moccasin = sRGB24 255 228 181

navajowhite :: (Ord a, Floating a) => Colour a
navajowhite = sRGB24 255 222 173

navy :: (Ord a, Floating a) => Colour a
navy = sRGB24 0 0 128

oldlace :: (Ord a, Floating a) => Colour a
oldlace = sRGB24 253 245 230

olive :: (Ord a, Floating a) => Colour a
olive = sRGB24 128 128 0

olivedrab :: (Ord a, Floating a) => Colour a
olivedrab = sRGB24 107 142 35

orange :: (Ord a, Floating a) => Colour a
orange = sRGB24 255 165 0

orangered :: (Ord a, Floating a) => Colour a
orangered = sRGB24 255 69 0

orchid :: (Ord a, Floating a) => Colour a
orchid = sRGB24 218 112 214

palegoldenrod :: (Ord a, Floating a) => Colour a
palegoldenrod = sRGB24 238 232 170

palegreen :: (Ord a, Floating a) => Colour a
palegreen = sRGB24 152 251 152

paleturquoise :: (Ord a, Floating a) => Colour a
paleturquoise = sRGB24 175 238 238

palevioletred :: (Ord a, Floating a) => Colour a
palevioletred = sRGB24 219 112 147

papayawhip :: (Ord a, Floating a) => Colour a
papayawhip = sRGB24 255 239 213

peachpuff :: (Ord a, Floating a) => Colour a
peachpuff = sRGB24 255 218 185

peru :: (Ord a, Floating a) => Colour a
peru = sRGB24 205 133 63

pink :: (Ord a, Floating a) => Colour a
pink = sRGB24 255 192 203

plum :: (Ord a, Floating a) => Colour a
plum = sRGB24 221 160 221

powderblue :: (Ord a, Floating a) => Colour a
powderblue = sRGB24 176 224 230

purple :: (Ord a, Floating a) => Colour a
purple = sRGB24 128 0 128

red :: (Ord a, Floating a) => Colour a
red = sRGB24 255 0 0

rosybrown :: (Ord a, Floating a) => Colour a
rosybrown = sRGB24 188 143 143

royalblue :: (Ord a, Floating a) => Colour a
royalblue = sRGB24 65 105 225

saddlebrown :: (Ord a, Floating a) => Colour a
saddlebrown = sRGB24 139 69 19

salmon :: (Ord a, Floating a) => Colour a
salmon = sRGB24 250 128 114

sandybrown :: (Ord a, Floating a) => Colour a
sandybrown = sRGB24 244 164 96

seagreen :: (Ord a, Floating a) => Colour a
seagreen = sRGB24 46 139 87

seashell :: (Ord a, Floating a) => Colour a
seashell = sRGB24 255 245 238

sienna :: (Ord a, Floating a) => Colour a
sienna = sRGB24 160 82 45

silver :: (Ord a, Floating a) => Colour a
silver = sRGB24 192 192 192

skyblue :: (Ord a, Floating a) => Colour a
skyblue = sRGB24 135 206 235

slateblue :: (Ord a, Floating a) => Colour a
slateblue = sRGB24 106 90 205

slategray :: (Ord a, Floating a) => Colour a
slategray = sRGB24 112 128 144

slategrey :: (Ord a, Floating a) => Colour a
slategrey = sRGB24 112 128 144

snow :: (Ord a, Floating a) => Colour a
snow = sRGB24 255 250 250

springgreen :: (Ord a, Floating a) => Colour a
springgreen = sRGB24 0 255 127

steelblue :: (Ord a, Floating a) => Colour a
steelblue = sRGB24 70 130 180

tan :: (Ord a, Floating a) => Colour a
tan = sRGB24 210 180 140

teal :: (Ord a, Floating a) => Colour a
teal = sRGB24 0 128 128

thistle :: (Ord a, Floating a) => Colour a
thistle = sRGB24 216 191 216

tomato :: (Ord a, Floating a) => Colour a
tomato = sRGB24 255 99 71

turquoise :: (Ord a, Floating a) => Colour a
turquoise = sRGB24 64 224 208

violet :: (Ord a, Floating a) => Colour a
violet = sRGB24 238 130 238

wheat :: (Ord a, Floating a) => Colour a
wheat = sRGB24 245 222 179

white :: (Ord a, Floating a) => Colour a
white = sRGB24 255 255 255

whitesmoke :: (Ord a, Floating a) => Colour a
whitesmoke = sRGB24 245 245 245

yellow :: (Ord a, Floating a) => Colour a
yellow = sRGB24 255 255 0

yellowgreen :: (Ord a, Floating a) => Colour a
yellowgreen = sRGB24 154 205 50