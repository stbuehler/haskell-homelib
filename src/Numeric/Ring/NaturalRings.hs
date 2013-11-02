{-# LANGUAGE TemplateHaskell #-}

module Numeric.Ring.NaturalRings
	( NaturalRing1
	, NaturalRing2
	, NaturalRing3
	, NaturalRing4
	, NaturalRing5
	, NaturalRing6
	, NaturalRing7
	, NaturalRing8
	, NaturalRing9
	, NaturalRing10
	, NaturalRing11
	, NaturalRing12
	, NaturalRing13
	, NaturalRing14
	, NaturalRing15
	, NaturalRing16
	, NaturalRing17
	, NaturalRing18
	, NaturalRing19
	, NaturalRing20
	, NaturalRing1000000000
	) where

import Numeric.Ring.GenNaturalRing

defineNaturalRings [1..20]
defineNaturalRing (10^(9::Int))
