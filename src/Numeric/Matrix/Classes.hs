{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Numeric.Matrix.Classes
	( Vector(..)
	, Matrix(..)
	, dotProduct
	, matrixProduct
	, matrixMultVector
	, vectorMultMatrix

	, AnyMatrix
	, toAnyMatrix
	, rowToAnyMatrix
	, colToAnyMatrix
	, anyMatrixDimensions
	, anyMatrixFromScalar
	, anyMatrixFromRowList
	, anyMatrixFromRowList'
	, anyMatrixRowList
	, anyMatrixSizedFromScalar
	) where

import Data.Tagged
import Data.List
import qualified Data.Array.IArray as A

class Vector v e | v -> e where
	vectorToList :: v -> [e]
	vectorFromList :: [e] -> v
	vectorSize :: Tagged v Int

class (Vector r e, Vector c e) => Matrix m r c e | m -> r, m -> c, m -> e where
	toRowVectors :: m -> [r]
	toColVectors :: m -> [c]
	fromRowVectors :: [r] -> m
	fromColVectors :: [c] -> m
	matrixDimensions :: Tagged m (Int, Int)

dotProduct :: (Vector v e, Num e) => v -> v -> e
dotProduct a b = sum $ zipWith (*) (vectorToList a) (vectorToList b)

map' :: (a -> b) -> [a] -> [b]
map' f l = let l' = map f l in foldl' (flip seq) l' l'

matrixProduct :: (Num e, Matrix m1 k c e, Matrix m2 r k e, Matrix m3 r c e) => m1 -> m2 -> m3
matrixProduct a b = fromRowVectors $ map' (\r -> vectorFromList $ map' (dotProduct r) $ toColVectors b) $ toRowVectors a

matrixMultVector :: (Num e, Matrix m r c e) => m -> r -> c
matrixMultVector a b = vectorFromList $ map (\r -> dotProduct r b) $ toRowVectors a
vectorMultMatrix :: (Num e, Matrix m r c e) => c -> m -> r
vectorMultMatrix a b = vectorFromList $ map (\c -> dotProduct a c) $ toColVectors b

data AnyMatrix e = AnyScalar !e | AnyMatrix !(A.Array (Int, Int) e)
instance Functor AnyMatrix where
	fmap f (AnyScalar a) = AnyScalar $ f a
	fmap f (AnyMatrix a) = AnyMatrix $ fmap f a

toAnyMatrix :: (Matrix m r c e) => m -> AnyMatrix e
toAnyMatrix m = anyMatrixFromRowList' (matrixDimensions `witness` m) $ map vectorToList $ toRowVectors m
rowToAnyMatrix :: (Vector r e) => r -> AnyMatrix e
rowToAnyMatrix r = anyMatrixFromRowList' (1, vectorSize `witness` r) [vectorToList r]
colToAnyMatrix :: (Vector c e) => c -> AnyMatrix e
colToAnyMatrix c = anyMatrixFromRowList' (vectorSize `witness` c, 1) $ map (\x -> [x]) $ vectorToList c

_dim :: A.Array (Int, Int) e -> (Int, Int)
_dim a = let ((1, 1), d) = A.bounds a in d

anyMatrixDimensions :: AnyMatrix e -> Maybe (Int, Int)
anyMatrixDimensions (AnyScalar _ ) = Nothing
anyMatrixDimensions (AnyMatrix a) = Just $ _dim a

anyMatrixFromScalar :: e -> AnyMatrix e
anyMatrixFromScalar = AnyScalar

anyMatrixFromRowList :: [[e]] -> AnyMatrix e
anyMatrixFromRowList l = anyMatrixFromRowList' (length l, length $ head l) l

anyMatrixFromRowList' :: (Int, Int) -> [[e]] -> AnyMatrix e
anyMatrixFromRowList' (m, n) l = AnyMatrix $ A.array ((1,1),(m,n)) $ concat $ zipWith (\i r -> map (\(j, e) -> ((i, j), e)) $ zipWith (,) [1..n] r) [1..m] l

anyMatrixRowList :: AnyMatrix e -> [[e]]
anyMatrixRowList (AnyScalar e) = [[e]]
anyMatrixRowList (AnyMatrix a) = let (m, n) = _dim a in [ [ a A.! (i, j) | j <- [1..n]] | i <- [1..m] ]

anyMatrixSizedFromScalar :: Num e => (Int, Int) -> e -> AnyMatrix e
anyMatrixSizedFromScalar (m, n) s = let dim = ((1,1),(m,n)) in AnyMatrix $ A.array dim $ map (\(i, j) -> ((i, j), if i == j then s else 0)) $ A.range dim

_matrixArrayZipWith :: (a -> b -> c) -> A.Array (Int, Int) a -> A.Array (Int, Int) b -> A.Array (Int, Int) c
_matrixArrayZipWith f a b = A.array (A.bounds a) $ zipWith z (A.assocs a) (A.assocs b) where
	z ((i1, j1), e1) ((i2, j2), e2) = if i1 == i2 && j1 == j2 then ((i1, j2), f e1 e2) else error "matrix dimension don't match"

instance Num e => Num (AnyMatrix e) where
	AnyScalar a + AnyScalar b = AnyScalar $ a + b
	AnyScalar a + AnyMatrix b = AnyMatrix $ A.array (A.bounds b) $ map (\((i, j), e) -> ((i, j), if i == j then a + e else e)) $ A.assocs b
	AnyMatrix a + AnyScalar b = AnyMatrix $ A.array (A.bounds a) $ map (\((i, j), e) -> ((i, j), if i == j then e + b else e)) $ A.assocs a
	AnyMatrix a + AnyMatrix b = AnyMatrix $ _matrixArrayZipWith (+) a b
	AnyScalar a - AnyScalar b = AnyScalar $ a - b
	AnyScalar a - AnyMatrix b = AnyMatrix $ A.array (A.bounds b) $ map (\((i, j), e) -> ((i, j), if i == j then a - e else e)) $ A.assocs b
	AnyMatrix a - AnyScalar b = AnyMatrix $ A.array (A.bounds a) $ map (\((i, j), e) -> ((i, j), if i == j then e - b else e)) $ A.assocs a
	AnyMatrix a - AnyMatrix b = AnyMatrix $ _matrixArrayZipWith (-) a b
	AnyScalar a * AnyScalar b = AnyScalar $ a * b
	AnyScalar a * AnyMatrix b = AnyMatrix $ fmap (a*) b
	AnyMatrix a * AnyScalar b = AnyMatrix $ fmap (*b) a
	AnyMatrix a * AnyMatrix b = checkdim $ AnyMatrix $ A.array dim [ (pos, mult pos) | pos <- A.range dim ] where
		(l, m) = _dim a
		(m', n) = _dim b
		dim = ((1, 1), (l, n))
		mult (i, j) = sum [ a A.! (i, k) * b A.! (k, j) | k <- [1..m] ]
		checkdim x = if m == m' then x else error "matrix dimensions don't match"
	abs = id
	signum = const 1
	fromInteger n = AnyScalar $ fromInteger n

_anyMatrixShow :: Show e => AnyMatrix e -> String
_anyMatrixShow (AnyScalar s) = "< " ++ show s ++ " >"
_anyMatrixShow (AnyMatrix a) = concat rows where
	(m, n) = _dim a
	as = fmap show a
	al = fmap length as
	rl = [ maximum [al A.! (i, j) | i <- [1..m] ] | j <- [1..n] ]
	rows = zipWith (\(l, r) s -> l ++ s ++ r ++ "\n") sep $ [ concat $ zipWith pad rl [as A.! (i, j) | j <- [1..n]] | i <- [1..m] ]
	pad l s = replicate (1 + l - length s) ' ' ++ s
	sep = if m == 1 then [("<", " >")] else [("/", " \\")] ++ replicate (m-2) ("|", " |") ++ [("\\", " /")]

instance Show e => Show (AnyMatrix e) where
	show = _anyMatrixShow
