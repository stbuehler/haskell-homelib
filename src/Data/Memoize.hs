
module Data.Memoize
	( memoizeFix
	, memoizeInt
	, memoizeIntRadix
	, memoizeIntPositive
	, memoizeArr
	) where

import qualified Data.Array as A
import Data.Bits

-- use a memoizer to fix a function
memoizeFix :: ((a -> b) -> a -> b) -> ((a -> b) -> a -> b) -> a -> b
memoizeFix mem f = let mf = mem (f mf) in mf

memoizeInt :: (Bits n, Num n, Ord n) => (n -> a) -> n -> a
memoizeInt f = integerLookup $ fmap f integerGenerate

memoizeIntRadix :: (Bits n, Num n) => (n -> a) -> n -> a
memoizeIntRadix f = radixLookup $ fmap f radixGenerate

memoizeIntPositive :: (Bits n, Num n, Ord n) => (n -> a) -> n -> a
memoizeIntPositive f = (\m n -> if n <= 0 then f n else positiveLookup m n) $ fmap f positiveGenerate

memoizeArr :: (A.Ix i) => (i, i) -> (i -> a) -> i -> a
memoizeArr bounds f = (\x -> if (A.inRange bounds x) then arr A.! x else f x)
	where
		arr = A.listArray bounds (map f $ A.range bounds)

-- radix tree from least significant bit
-- node has 4 parts:
--   "a0"  : value if all other bits are 0, i.e. value = 0 (positive values)
--   "am1" : value if all other bits are 1, i.e. value = -1 (negative values)
--   "left": if next bit is 0
--   "right": if next bit is 1
data RadixTree a = RadixNode a a (RadixTree a) (RadixTree a)
instance Functor RadixTree where
	fmap f (RadixNode a0 am1 tl tr) = RadixNode (f a0) (f am1) (fmap f tl) (fmap f tr)

-- quickCheck (\n -> radixLookup radixGenerate n == n)
radixGenerate :: (Bits n, Num n) => RadixTree n
radixGenerate = RadixNode 0 (-1) (fmap (flip shiftL 1) radixGenerate) (fmap ((1 `xor`) . flip shiftL 1) radixGenerate)

radixLookup :: (Bits n, Num n) => RadixTree v -> n -> v
radixLookup (RadixNode v _ _ _)   0  = v
radixLookup (RadixNode _ v _ _) (-1) = v
radixLookup (RadixNode _ _ l r) n = let n2 = shiftR n 1 in n2 `seq` if testBit n 0 then radixLookup r n2 else radixLookup l n2


data PositiveTree a = PositiveNode a (PositiveTree a) (PositiveTree a)
instance Functor PositiveTree where
	fmap f (PositiveNode v tl tr) = PositiveNode (f v) (fmap f tl) (fmap f tr)

-- quickCheck (\n -> n <= 0 || positiveLookup positiveGenerate n == n)
positiveGenerate :: (Bits n, Num n) => PositiveTree n
positiveGenerate = PositiveNode 1 (fmap (flip shiftL 1) positiveGenerate) (fmap ((1 `xor`) . flip shiftL 1) positiveGenerate)

positiveLookup :: (Bits n, Num n) => PositiveTree v -> n -> v
positiveLookup (PositiveNode v _ _) 1 = v
positiveLookup (PositiveNode _ l r) n = let n2 = shiftR n 1 in n2 `seq` if testBit n 0 then positiveLookup r n2 else positiveLookup l n2

newtype IntegerCache a = IntegerCache (PositiveTree a)
instance Functor IntegerCache where
	fmap f (IntegerCache c) = IntegerCache $ fmap f c

-- quickCheck (\n -> integerLookup integerGenerate n == n)
integerGenerate :: (Bits n, Num n) => IntegerCache n
integerGenerate = IntegerCache $ PositiveNode 0 (fmap negate positiveGenerate) positiveGenerate

integerLookup :: (Bits n, Num n, Ord n) => IntegerCache v -> n -> v
integerLookup (IntegerCache (PositiveNode v0 tn tp)) n = case compare n 0 of
	LT -> positiveLookup tn (negate n)
	EQ -> v0
	GT -> positiveLookup tp n
