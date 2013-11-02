{-# LANGUAGE FlexibleInstances #-}

module Numeric.Field.Classes
	( Field(..)
	, FiniteField(..)
	, PrimeField
	) where

class Fractional n => Field n where
	characteristic :: n -> Integer

class (Integral n, Field n) => FiniteField n where
	fieldSize :: n -> Integer -- characteristic ^ e for some natural e

instance Field Rational where
	characteristic = const 0

class (FiniteField n, Bounded n) => PrimeField n where
