{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Numeric.Matrix.DefaultMatrices
	( Vector1(..)
	, Vector2(..)
	, Vector3(..)
	, Vector4(..)
	, Vector5(..)

	, Matrix1x1(..)
	, Matrix2x2(..)
	, Matrix3x3(..)
	, Matrix4x4(..)
	, Matrix5x5(..)

	) where

import Numeric.Matrix.GenMatrix

manyDecs [defineVector n | n <- [1..5]]
manyDecs [defineMatrix n n | n <- [1..5]]
