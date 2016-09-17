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

	, NaturalRing100
	, NaturalRing1000
	, NaturalRing10000
	, NaturalRing100000
	, NaturalRing1000000
	, NaturalRing10000000
	, NaturalRing100000000
	, NaturalRing1000000000
	, NaturalRing10000000000
	) where

import Numeric.Ring.GenNaturalRing

defineNaturalRings [1..20]
defineNaturalRings $ map (10^) [2..10::Int]
