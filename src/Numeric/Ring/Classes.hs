
module Numeric.Ring.Classes
	( Ring(..)
	, FiniteRing(..)
	) where

class Num n => Ring n where
	ringRecip :: n -> Maybe n

class Ring n => FiniteRing n where
	ringSize :: n -> Integer
