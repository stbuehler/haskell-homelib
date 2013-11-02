{-# OPTIONS -fdicts-cheap #-}
{-# LANGUAGE BangPatterns #-}

module Numeric.Primes (primes, isPrime, genPrimeArray, wheel, phi, factors, divisors) where

-- import Data.Array
import qualified Data.Array.IArray as IA

{- SPECIALIZE INLINE primes :: [CInt] #-}
{- SPECIALIZE INLINE isPrime :: CInt -> Bool #-}
{- SPECIALIZE INLINE primes :: [Int] #-}
{- SPECIALIZE INLINE isPrime :: Int -> Bool #-}
{- SPECIALIZE INLINE primes :: [Integer] #-}
{- SPECIALIZE INLINE isPrime :: Integer -> Bool #-}

primes :: Integral n => [n]
primes = 2:filter isPrime (tail wheel2357)
isPrime :: Integral n => n -> Bool
isPrime x = foldr _prime_f True primes
	where
	_prime_f p r = x < p*p || mod x p /= 0 && r

-- primesOld :: Integral n => [n]
-- primesOld = 2:filter isPrimeOld [3,5..]
-- isPrimeOld x = foldr (_prime_fOld x) True primesOld
-- 	where
-- 	_prime_fOld x p r = x < p*p || mod x p /= 0 && r

genPrimeArray :: (Integral n, IA.Ix n) => n -> IA.Array n Bool
genPrimeArray n = pa where
	pa = IA.array (1,n) $ (1,False):[ (i, localIsPrime i) | i <- [2..n] ] -- :: Array Int Bool
	localIsPrime x = any (\i -> mod x i /= 0) $ takeWhile (\p -> p <= x*x) $ localPrimes
	localPrimes = map fst $ filter snd $ IA.assocs pa

wheel2357 :: Integral n => [n]
wheel2357 = wheel [2,3,5,7]
wheel :: Integral n => [n] -> [n]
wheel initPrimes = initPrimes ++ spin start (cycle $ zipWith (-) (l) (start:l))
	where
		spin = scanl (+)
		start = head $ filter (\n -> all (\p -> n `mod` p > 0) initPrimes) [1+maximum initPrimes..]
		len = product initPrimes
		l = filter (\n -> all (\p -> n `mod` p > 0) initPrimes) [start+1..start+len]

phi :: Integer -> Integer
phi = _phi primes
	where
	_phi _ 1 = 1
	_phi (p:ps) !n = if (p*p > n) then (n-1) else if (n `mod` p > 0)
		then _phi ps n
		else case (n `div` p) of n1 -> (if (n1 `mod` p == 0) then p else (p-1)) * _phi (p:ps) n1
	_phi [] _ = error "not enough primes"

factors :: (Integral n) => [n] -> n -> [n]
factors _ 0 = [0]
factors _ 1 = []
factors (p:xp) n = if (p*p > n) then [n] else if (n `mod` p == 0) then (p:factors xp (n `div` p)) else (p:factors xp n)
factors [] _ = error "not enough primes to factorize"

divisors :: (Integral n) => n -> [n]
divisors = _d 2 where
	_d d n = if (d*d > n) then [] else if (n `mod` d == 0) then d:_d (d+1) n else _d (d+1) n
