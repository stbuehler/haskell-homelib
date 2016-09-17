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

class Integral n => InternalNaturalRing n where
	inrSize :: Tagged n Integer

inrRecip :: InternalNaturalRing n => n -> Maybe n
inrRecip n = case invertMod (toInteger n) (inrSize `witness` n) of
	Just x -> Just $ fromInteger x
	Nothing -> Nothing

-- type + constructor name
naturalRingName :: Integer -> Name
naturalRingName base = mkName $ "NaturalRing" ++ show base

-- (single) field name + "destructor" name
naturalringName :: Integer -> Name
naturalringName base = mkName $ "naturalRing" ++ show base

-- constructor with `mod` base wrapping name
buildNaturalRingName :: Integer -> Name
buildNaturalRingName base = mkName $ "buildNaturalRing" ++ show base

-- multiplication must fit in the same type
baseType :: Integer -> Q Type
baseType base
	| base < 2^(16::Int) = [t|Word32|]
	| base < 2^(32::Int) = [t|Word64|]
	| otherwise = [t|Integer|]

_simpleInline :: Name -> Q Dec
_simpleInline n = return $ PragmaD $ InlineP n Inline FunLike AllPhases

genNaturalRingNewType :: Integer -> Q [Dec]
genNaturalRingNewType base = do
	let tname = naturalRingName base -- type + constructor tname
	let fname = naturalringName base -- field name ("destructor")
	let bname = buildNaturalRingName base -- construct after mod
	-- newtype $tname = $tname { $fname :: Integer } deriving (Eq)
	nt <- newtypeD (cxt []) tname [] (recC tname [varStrictType fname $ strictType notStrict $ baseType base ]) [''Eq]
	bfuncsig <- sigD bname [t|$(baseType base) -> $(conT tname)|]
	bfunc <- funD bname [ clause [] (normalB [e|\i -> $(conE tname) $ i `mod` (fromInteger base)|]) []]
	inlinebfunc <- _simpleInline bname
	i <- [d|
		instance InternalNaturalRing $(conT tname) where
			inrSize = Tagged base
		|]
	return $ nt:bfuncsig:bfunc:inlinebfunc:i

defineNaturalRing :: Integer -> Q [Dec]
defineNaturalRing base = do
	let tname = naturalRingName base
	let fname = naturalringName base -- field name ("destructor")
	let bname = buildNaturalRingName base -- construct after mod
	t <- genNaturalRingNewType base
	i <- [d|
		instance Show $(conT tname) where
			show = show . $(varE fname)
		instance Num $(conT tname) where
			a + b = $(varE bname) $ $(varE fname) a + $(varE fname) b
			{-# INLINE (+) #-}
			a - b = $(varE bname) $ $(varE fname) a - $(varE fname) b
			{-# INLINE (-) #-}
			a * b = $(varE bname) $ $(varE fname) a * $(varE fname) b
			{-# INLINE (*) #-}
			negate 0 = 0
			negate n = $(conE tname) $ fromInteger base - $(varE fname) n
			{-# INLINE negate #-}
			-- x == abs x / signum x for gcd
			-- abs 0 = 0
			-- abs _ = 1
			-- signum 0 = 0
			-- signum n = 1 / n
			-- x == abs x * signum x
			abs = const 1
			{-# INLINE abs #-}
			signum = id
			{-# INLINE signum #-}
			fromInteger n = $(conE tname) $ fromInteger $ n `mod` base
			{-# INLINE fromInteger #-}
		instance Ord $(conT tname) where
			compare = comparing $(varE fname)
			{-# INLINE compare #-}
		instance Real $(conT tname) where
			toRational = toRational . $(varE fname)
			{-# INLINE toRational #-}
		instance Enum $(conT tname) where
			toEnum = $(varE bname) . toEnum
			{-# INLINE toEnum #-}
			fromEnum = fromEnum . $(varE fname)
			{-# INLINE fromEnum #-}
			succ = $(varE bname) . (+1) . $(varE fname)
			{-# INLINE succ #-}
			pred = $(varE bname) . (flip (-) 1) . $(varE fname)
			{-# INLINE pred #-}
		instance Integral $(conT tname) where
			toInteger = toInteger . $(varE fname)
			{-# INLINE toInteger #-}
			quotRem _ b = (0, b)
			divMod = quotRem
		-- instance Fractional $(conT tname) where
		-- 	a / b = a * inrInvert b
		-- 	recip = const 0
		-- 	fromRational r = $(varE bname) (numerator r) / $(varE bname) (denominator r)
		instance Bounded $(conT tname) where
			minBound = 0
			maxBound = $(varE bname) $ base - 1
		instance Ring $(conT tname) where
			ringRecip = inrRecip
		instance FiniteRing $(conT tname) where
			ringSize = witness inrSize
		instance NFData $(conT tname) where
			rnf x = seq x ()
		|]
	return $ t ++ i

defineNaturalRings :: [Integer] -> Q [Dec]
defineNaturalRings = fmap concat . mapM defineNaturalRing
