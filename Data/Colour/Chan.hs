module Data.Colour.Chan where
{- For internal use only:
   Not to be exported from the package -}
newtype Chan p a = Chan a deriving (Eq)

empty :: (Num a) => Chan p a
empty = Chan 0

full :: (Num a) => Chan p a
full = Chan 1

scale :: (Num a) => a -> Chan p a -> Chan p a
scale s (Chan x) =  Chan (s*x)

add :: (Num a) => Chan p a -> Chan p a -> Chan p a
(Chan a) `add` (Chan b) = Chan (a+b)

invert :: (Num a) => Chan p a -> Chan p a
invert (Chan a) = Chan (1-a)

over c0 a c1 = c0 `add` scale (1-a) c1

convert :: (Fractional b, Real a) => Chan p a -> Chan p b
convert (Chan x) = Chan (realToFrac x)