{-# OPTIONS -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Numeric.Field.GenPrimeField
	( definePrimeField
	, definePrimeFields
	) where

import Numeric.Field.Classes

import Data.Tagged
import Data.Ord
import Data.Ratio
import Control.DeepSeq
import Math.NumberTheory.Moduli
import Language.Haskell.TH

class InternalPrimeField n where
	ipfConstruct :: Integer -> n
	ipfDestruct :: n -> Integer
	ipfSize :: Tagged n Integer

ipfInvert :: InternalPrimeField n => n -> n
ipfInvert n = case invertMod (ipfDestruct n) (ipfSize `witness` n) of
	Just x -> ipfConstruct x
	Nothing -> error "divide by zero"

primeFieldName :: Integer -> Name
primeFieldName prime = mkName $ "PrimeField" ++ show prime

primefieldName :: Integer -> Name
primefieldName prime = mkName $ "primeField" ++ show prime

genPrimeFieldNewType :: Integer -> Q [Dec]
genPrimeFieldNewType prime = do
	let name = primeFieldName prime -- type + constructor name
	let fname = primefieldName prime -- field name ("destructor")
	-- newtype $name = $name { $fname :: Integer } deriving (Eq)
	let baset = if prime < 2^(15::Int) then [t|Word|] else [t|Integer|]
	nt <- newtypeD (cxt []) name [] (recC name [varStrictType fname $ strictType notStrict baset ]) [''Eq]
	i <- [d|
		instance InternalPrimeField $(conT name) where
			ipfConstruct n = $(conE name) $ fromInteger $ n `mod` prime
			ipfDestruct = toInteger . $(varE fname)
			ipfSize = Tagged prime
		|]
	return $ nt:i

definePrimeField :: Integer -> Q [Dec]
definePrimeField prime = do
	let tname = primeFieldName prime
	t <- genPrimeFieldNewType prime
	i <- [d|
		instance Show $(conT tname) where
			show = show . ipfDestruct
		instance Num $(conT tname) where
			a + b = ipfConstruct $ ipfDestruct a + ipfDestruct b
			a - b = ipfConstruct $ ipfDestruct a - ipfDestruct b
			a * b = ipfConstruct $ ipfDestruct a * ipfDestruct b
			negate = ipfConstruct . negate . ipfDestruct
			-- x == abs x / signum x for gcd
			-- abs 0 = 0
			-- abs _ = 1
			-- signum 0 = 0
			-- signum n = 1 / n
			-- x == abs x * signum x
			abs = const 1
			signum = id
			fromInteger = ipfConstruct
		instance Ord $(conT tname) where
			compare = comparing ipfDestruct
		instance Real $(conT tname) where
			toRational = toRational . ipfDestruct
		instance Enum $(conT tname) where
			toEnum = ipfConstruct . toEnum
			fromEnum = fromEnum . ipfDestruct
			succ = ipfConstruct . (+1) . ipfDestruct
			pred = ipfConstruct . (flip (-) 1) . ipfDestruct
		instance Integral $(conT tname) where
			toInteger = ipfDestruct
			quotRem a b = (a * ipfInvert b, 0)
			divMod = quotRem
		instance Fractional $(conT tname) where
			a / b = a * ipfInvert b
			recip = const 0
			fromRational r = ipfConstruct (numerator r) / ipfConstruct (denominator r)
		instance Bounded $(conT tname) where
			minBound = 0
			maxBound = ipfConstruct $ prime - 1
		instance Field $(conT tname) where
			characteristic = witness ipfSize
		instance FiniteField $(conT tname) where
			fieldSize = characteristic
		instance PrimeField $(conT tname) where
		instance NFData $(conT tname) where
			rnf x = seq x ()
		|]
	return $ t ++ i

definePrimeFields :: [Integer] -> Q [Dec]
definePrimeFields = fmap concat . mapM definePrimeField
