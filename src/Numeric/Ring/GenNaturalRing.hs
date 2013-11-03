{-# OPTIONS -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Numeric.Ring.GenNaturalRing
	( defineNaturalRing
	, defineNaturalRings
	) where

import Numeric.Ring.Classes

import Data.Tagged
import Data.Ord
import Data.Word
import Math.NumberTheory.Moduli
import Control.DeepSeq
import Language.Haskell.TH

class InternalNaturalRing n where
	inrConstruct :: Integer -> n
	inrDestruct :: n -> Integer
	inrSize :: Tagged n Integer

inrRecip :: InternalNaturalRing n => n -> Maybe n
inrRecip n = case invertMod (inrDestruct n) (inrSize `witness` n) of
	Just x -> Just $ inrConstruct x
	Nothing -> Nothing

naturalRingName :: Integer -> Name
naturalRingName base = mkName $ "NaturalRing" ++ show base

naturalringName :: Integer -> Name
naturalringName base = mkName $ "naturalRing" ++ show base

genNaturalRingNewType :: Integer -> Q [Dec]
genNaturalRingNewType base = do
	let name = naturalRingName base -- type + constructor name
	let fname = naturalringName base -- field name ("destructor")
	-- newtype $name = $name { $fname :: Integer } deriving (Eq)
	let baset = if base < 2^(15::Int) then [t|Word|] else [t|Integer|]
	nt <- newtypeD (cxt []) name [] (recC name [varStrictType fname $ strictType notStrict baset ]) [''Eq]
	i <- [d|
		instance InternalNaturalRing $(conT name) where
			inrConstruct n = $(conE name) $ fromInteger $ n `mod` base
			inrDestruct = toInteger . $(varE fname)
			inrSize = Tagged base
		|]
	return $ nt:i

defineNaturalRing :: Integer -> Q [Dec]
defineNaturalRing base = do
	let tname = naturalRingName base
	t <- genNaturalRingNewType base
	i <- [d|
		instance Show $(conT tname) where
			show = show . inrDestruct
		instance Num $(conT tname) where
			a + b = inrConstruct $ inrDestruct a + inrDestruct b
			a - b = inrConstruct $ inrDestruct a - inrDestruct b
			a * b = inrConstruct $ inrDestruct a * inrDestruct b
			negate = inrConstruct . negate . inrDestruct
			-- x == abs x / signum x for gcd
			-- abs 0 = 0
			-- abs _ = 1
			-- signum 0 = 0
			-- signum n = 1 / n
			-- x == abs x * signum x
			abs = const 1
			signum = id
			fromInteger = inrConstruct
		instance Ord $(conT tname) where
			compare = comparing inrDestruct
		instance Real $(conT tname) where
			toRational = toRational . inrDestruct
		instance Enum $(conT tname) where
			toEnum = inrConstruct . toEnum
			fromEnum = fromEnum . inrDestruct
			succ = inrConstruct . (+1) . inrDestruct
			pred = inrConstruct . (flip (-) 1) . inrDestruct
		instance Integral $(conT tname) where
			toInteger = inrDestruct
			quotRem _ b = (0, b)
			divMod = quotRem
		-- instance Fractional $(conT tname) where
		-- 	a / b = a * inrInvert b
		-- 	recip = const 0
		-- 	fromRational r = inrConstruct (numerator r) / inrConstruct (denominator r)
		instance Bounded $(conT tname) where
			minBound = 0
			maxBound = inrConstruct $ base - 1
		instance Ring $(conT tname) where
			ringRecip = inrRecip
		instance FiniteRing $(conT tname) where
			ringSize = witness inrSize
		instance NFData $(conT tname) where
		|]
	return $ t ++ i

defineNaturalRings :: [Integer] -> Q [Dec]
defineNaturalRings = fmap concat . mapM defineNaturalRing
