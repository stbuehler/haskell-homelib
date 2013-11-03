{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Numeric.Matrix.DefaultMatrices
	( Vector1(..)
	, vector1
	, Vector2(..)
	, vector2
	, Vector3(..)
	, vector3
	, Vector4(..)
	, vector4
	, Vector5(..)
	, vector5

	, Matrix1x1(..)
	, matrix1x1
	, Matrix2x2(..)
	, matrix2x2
	, Matrix3x3(..)
	, matrix3x3
	, Matrix4x4(..)
	, matrix4x4
	, Matrix5x5(..)
	, matrix5x5

	) where

import Numeric.Matrix.GenMatrix

manyDecs [defineVector n | n <- [1..5]]
manyDecs [defineMatrix n n | n <- [1..5]]

-- check asymmetric stuff:
-- defineMatrix 3 4
-- defineMatrix 4 3
-- declareMatrixMult 4 3 4
-- declareMatrixMult 3 4 3
