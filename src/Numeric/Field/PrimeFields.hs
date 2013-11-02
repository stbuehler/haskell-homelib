{-# LANGUAGE TemplateHaskell #-}

module Numeric.Field.PrimeFields
	( PrimeField2
	, PrimeField3
	, PrimeField5
	, PrimeField7
	, PrimeField11
	, PrimeField13
	, PrimeField17
	, PrimeField19
	, PrimeField23
	, PrimeField29
	, PrimeField31
	, PrimeField37
	, PrimeField41
	, PrimeField43
	, PrimeField47
	, PrimeField53
	, PrimeField59
	, PrimeField61
	, PrimeField67
	, PrimeField71
	, PrimeField73
	, PrimeField79
	, PrimeField83
	, PrimeField89
	, PrimeField97
	) where

import Numeric.Field.GenPrimeField

definePrimeFields [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
