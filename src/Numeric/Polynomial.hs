
module Numeric.Polynomial
	( Polynomial

	, polynomialReduction
	, polynomialNorm
	, polynomialFieldGCD
	, polynomialIntegralGCD
	, polynomialMult
	, polynomialLeading
	, polynomialDegree
	, polynomialEntries
	, polynomialAggregateSort
	, polynomial
	, polynomial1
	, polynomialX
	, toPolynomial
	, polynomialDerive
	, polynomialSFF
	, polynomialDFF

	) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Word
import Data.List
import Data.Ord
import Data.Ratio

import Numeric.Field.Classes

-- start with largest exponent: a_d*X^d + ... + a_0*X^0: [(d, a_d), ..., (0, a_0)]
-- store only non-null entries
newtype Polynomial n = Polynomial (V.Vector (Word, n)) deriving (Eq)

instance (Show n, Num n, Eq n) => Show (Polynomial n) where
	show p = case polynomialEntries p of
			[] -> "0"
			x:xs -> s' x ++ concatMap go xs
		where
		go (e, n) = if '-' == head (s (e, n)) then '-':s (e, -n) else '+':s (e, n)
		s' (e, n) = if '-' == head (s (e, n)) then '-':s (e, -n) else s (e, n)
		s (0, n) = show n
		s (1, 1) = "x"
		s (1, n) = show n ++ "*x"
		s (e, 1) = "x^" ++ show e
		s (e, n) = show n ++ "*x^" ++ show e

instance (Num n, Eq n) => Num (Polynomial n) where
	a + b = polynomial $ polynomialEntries a ++ polynomialEntries b
	a - b = polynomial $ polynomialEntries a ++ (map (\(i,n) -> (i, negate n)) $ polynomialEntries b)
	a * b = polynomial $ [ (e1+e2, n1*n2) | (e1,n1) <- polynomialEntries a, (e2,n2) <- polynomialEntries b ]
	negate a = polynomial $ map (\(i,n) -> (i, negate n)) $ polynomialEntries a
	abs    = id
	signum 0 = 0
	signum _ = 1
	fromInteger n = polynomial [(0, fromInteger n)]


instance (Num n, Eq n) => Ord (Polynomial n) where
	compare = error "faked Ord"
instance (Num n, Eq n) => Real (Polynomial n) where
	toRational = error "faked Real"
instance (Num n, Eq n) => Enum (Polynomial n) where
	toEnum = fromInteger . toEnum
	fromEnum = error "faked Enum"
instance (Eq n, Fractional n) => Integral (Polynomial n) where
	toInteger _ = error "faked Integral"
	quotRem a b = if diff < 0 then (0, a)
			else if 0 == polynomialDegree a then (qp, 0)
			else if polynomialDegree a == polynomialDegree a' then error ("rounding problems")
			--else if polynomialDegree a == polynomialDegree a' then error ("rounding problems: " ++ show (a, a', polynomialLeading b, q, qp))
			else (q' + qp, r')
		where
		diff :: Integer
		diff = fromIntegral (polynomialDegree a) - fromIntegral (polynomialDegree b)
		q = polynomialLeading a / polynomialLeading b
		qp = polynomial [(fromInteger diff, q)]
		a' = a - qp * b
		(q', r') = a' `quotRem` b
	divMod = quotRem

lcM :: Integral n => [n] -> n
lcM = foldl' lcm 1
gcD :: Integral n => [n] -> n
gcD = foldl' gcd 0

polynomialReduction :: (Eq n, Integral n) => Polynomial (Ratio n) -> Polynomial n
polynomialReduction p = if all (1==) denoms' then p'' else error "denominators not all == 1" where
	-- we could end up with any unit in the denominators; but we can only handle 1 as we don't have a way to invert those units here
	denoms' = map (denominator . snd) $ polynomialEntries p'
	p'' = polynomial $ map (\(e, n) -> (e, numerator n)) $ polynomialEntries p'
	p' = polynomialMult (lcM denoms % gcD nums) p
	denoms = map (denominator . snd) $ polynomialEntries p
	nums = map (numerator . snd) $ polynomialEntries p

polynomialNorm :: (Eq n, Fractional n) => Polynomial n -> Polynomial n
polynomialNorm p = let l = polynomialLeading p in if 0 == l then 0 else polynomialMult (1/l) p

polynomialFieldGCD :: (Eq n, Fractional n) => Polynomial n -> Polynomial n -> Polynomial n
polynomialFieldGCD a 0 = polynomialNorm a
polynomialFieldGCD a b = polynomialFieldGCD b (a `mod` b)

polynomialIntegralGCD :: (Eq n, Integral n) => Polynomial n -> Polynomial n -> Polynomial n
polynomialIntegralGCD a b = polynomialMult c $ polynomialReduction c' where
	c = gcD $ map snd $ polynomialEntries a ++ polynomialEntries b
	a' = polynomial $ map (\(e, n) -> (e, n % 1)) $ polynomialEntries a
	b' = polynomial $ map (\(e, n) -> (e, n % 1)) $ polynomialEntries b
	c' = polynomialFieldGCD a' b'

polynomialMult :: (Num n, Eq n) => n -> Polynomial n -> Polynomial n
polynomialMult c p = polynomial $ map (\(e, n) -> (e, c*n)) $ polynomialEntries p

polynomialLeading :: (Num n) => Polynomial n => n
polynomialLeading (Polynomial a) = if V.null a then 0 else snd $ V.head a

polynomialDegree :: Polynomial n => Word
polynomialDegree (Polynomial a) = if V.null a then 0 else fst $ V.head a

polynomialEntries :: (Num n, Eq n) => Polynomial n -> [(Word, n)]
polynomialEntries (Polynomial a) = V.toList a

{-# INLINE polynomialAggregateSort #-}
polynomialAggregateSort :: (Num n, Eq n) => [(Word, n)] -> [(Word, n)]
polynomialAggregateSort = filter ((0 /= ) . snd) . undown . M.assocs . foldl' (\m (e, n) -> M.insertWith (+) e n m) M.empty . down
	where
	undown = map (\(Down e, n) -> (e, n))
	down = map (\(e, n) -> if e < 0 then error "invalid exponent" else (Down e, n))

polynomial :: (Num n, Eq n) => [(Word, n)] -> Polynomial n
polynomial = Polynomial . V.fromList . polynomialAggregateSort
--polynomial = Polynomial . V.fromList . filter ((0 /= ) . snd) . map (\l -> (fst (head l), sum $ map snd l)) . groupBy (\(e1, _) (e2, _) -> e1 == e2) . sortBy (flip $ comparing fst)

polynomial1 :: (Num n, Eq n) => [Word] -> Polynomial n
polynomial1 = polynomial . flip zip (repeat 1)

polynomialX :: (Num n, Eq n) => Polynomial n
polynomialX = polynomial [(1,1)]

toPolynomial :: (Num n, Eq n) => n -> Polynomial n
toPolynomial v = polynomial [(0, v)]

polynomialDerive :: (Num n, Eq n) => Polynomial n -> Polynomial n
polynomialDerive = polynomial . map (\(e, n) -> if e == 0 then (0,0) else (e - 1, fromIntegral e * n)) . polynomialEntries

-- factorize into square-free factors (which contain each root at most once)
polynomialSFF :: (Field n, Eq n) => Polynomial n -> [Polynomial n]
polynomialSFF p = if 0 == p' then sffroot p else sff 1 w0 c0
	where
	sff _ 1 1 = []
	sff _ 1 c = sffroot c
	sff i w c = i `seq` y `seq` c' `seq` r ++ sff (i+1) y c'  where
		r = if z == 1 then [] else replicate i z
		y = polynomialNorm $ polynomialFieldGCD w c
		(z, 0) = w `quotRem` y
		(c', 0) = c `quotRem` y
	char = characteristic (polynomialLeading p)
	p' = polynomialDerive p
	c0 = polynomialNorm $ polynomialFieldGCD p p'
	(w0, 0) = p `quotRem` c0
	sffroot = concat . replicate (fromIntegral char) . polynomialSFF . root
	root = polynomial . map (\(e, n) -> case e `quotRem` (fromIntegral char) of (e', 0) -> (e', n); _ -> error "expontent not multiple of char") . polynomialEntries

polynomialDFF :: (FiniteField n, Eq n) => Polynomial n -> [(Word, Polynomial n)]
polynomialDFF p = dff x 1 p
	where
	x = polynomialX
	dff _ _ 1 = []
	dff xqi i f = if d >= 2*i then if g == 1 then dff xqi' (i+1) f else (i, g):dff xqi' (i+1) (f `quot` g) else [(d, f)]
		where
		xqi' = xqi^q `mod` f
		d = polynomialDegree f
		g = polynomialNorm $ gcd f $ xqi' - x
	q = fieldSize (polynomialLeading p)
