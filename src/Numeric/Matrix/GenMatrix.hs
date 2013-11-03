{-# OPTIONS -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Numeric.Matrix.GenMatrix
	( defineVector
	, defineMatrix
	, declareMatrixMult
	, manyDecs
	) where

import Numeric.Matrix.Classes

import Language.Haskell.TH
import Data.Tagged
import Data.List
import Data.Data
import qualified Data.Foldable as F
import Data.Traversable (Traversable, traverse)
import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.DeepSeq

-- import Debug.Trace

manyDecs :: [Q [Dec]] -> Q [Dec]
manyDecs d = concat <$> sequence d

vectorName :: Int -> Name
vectorName n = mkName $ "Vector" ++ show n
vectorname :: Int -> Name
vectorname n = mkName $ "vector" ++ show n

defineVector :: Int -> Q [Dec]
defineVector n = do
	let name = vectorName n
	let e = mkName "e"
	let et = varT e
	let gennames c = map (\i -> mkName $ c ++ show i) [1..n]
	let enames = gennames "e"
	-- data VectorN e = VectorN !e !e ... !e
	nt <- dataD (cxt []) name [PlainTV e] [normalC name $ replicate n $ strictType isStrict et] [''Eq, ''Typeable, ''Data]
	inst <- instanceD (cxt []) (appT (conT ''Vector) $ conT name)
		[ funD 'vectorToList [clause [conP name $ map varP enames] (normalB $ listE $ map varE enames) []]
		, funD 'vectorFromList
			[ clause [listP $ map varP enames] (normalB $ foldl appE (conE name) $ map varE enames) []
			, clause [wildP] (normalB $ [e| error "list length doesn't match vector size" |]) []
			]
		, funD 'vectorSize [clause [] (normalB [e|Tagged n|]) []]
		, funD 'dotProduct [clause [conP name $ map varP $ gennames "a", conP name $ map varP $ gennames "b"]
			(normalB $ foldl1 (appE . appE (varE '(+))) $ zipWith (\a b -> foldl appE (varE '(*)) [varE a, varE b]) (gennames "a") (gennames "b")) [] ]
		]
	foldableinst <- instanceD (cxt []) (appT (conT ''F.Foldable) $ conT name)
		[ funD 'foldr [clause [varP $ mkName "f", varP $ mkName "z", conP name $ map varP enames] (normalB $ foldl appE (varE 'foldr) [varE $ mkName "f", varE $ mkName "z", listE $ map varE enames] ) []]
		]
	functorinst <- instanceD (cxt []) (appT (conT ''Functor) $ conT name)
		[ funD 'fmap [clause [varP $ mkName "f", conP name $ map varP enames] (normalB $ foldl appE (conE name) $ map (appE (varE $ mkName "f") . varE) enames) [] ]
		]
	applicativeinst <- instanceD (cxt []) (appT (conT ''Applicative) $ conT name)
		[ funD 'pure [clause [varP e] (normalB $ foldl appE (conE name) $ replicate n $ varE e) [] ]
		, funD '(<*>) [clause [conP name $ map varP $ gennames "f", conP name $ map varP enames]
			(normalB $ foldl appE (conE name) $ zipWith (\f x -> appE (varE f) $ varE x) (gennames "f") enames) [] ]
		]
	traversableinst <- instanceD (cxt []) (appT (conT ''Traversable) $ conT name)
		[ funD 'traverse [clause [varP $ mkName "f", conP name $ map varP enames] (normalB $ foldl (\x y -> appE (appE (varE '(<*>)) x) $ appE (varE $ mkName "f") (varE y)) (appE (varE 'pure) (conE name)) enames) []]
		]
	nfdatainst <- instanceD (cxt [classP ''NFData [et]]) (appT (conT ''NFData) $ appT (conT name) et)
		[ funD 'rnf [clause [conP name $ map varP enames] (normalB $ foldr (appE . appE (varE 'deepseq)) [e|()|] $ map varE enames) []]
		]
	showinst <- instanceD (cxt [classP ''Show [et]]) (appT (conT ''Show) $ appT (conT name) et)
		[ funD 'show [clause [] (normalB $ [e|show . rowToAnyMatrix|]) []]
		]
	sigconstr <- sigD (vectorname n) $ forallT [PlainTV e] (cxt []) $ foldl appT arrowT [foldl appT (tupleT n) $ replicate n et, appT (conT name) et]
	constr <- funD (vectorname n) [clause [tupP $ map varP enames] (normalB $ foldl appE (conE name) $ map varE enames) []]
	-- trace (pprint inst) $
	return $ [nt,inst,foldableinst,functorinst,applicativeinst,traversableinst,nfdatainst,showinst,sigconstr,constr]

matrixName :: Int -> Int -> Name
matrixName m n = mkName $ "Matrix" ++ show m ++ "x" ++ show n

matrixname :: Int -> Int -> Name
matrixname m n = mkName $ "matrix" ++ show m ++ "x" ++ show n

defineMatrix :: Int -> Int -> Q [Dec]
defineMatrix m n = do
	-- m rows, n cols. a row vector has n entries, a col vector m.
	let name = matrixName m n
	let e = mkName "e"
	let et = varT e
	let rownames = map (\r -> mkName $ "r" ++ show r) [1..m]
	let gennames c = [ [ mkName $ c ++ show i ++ "_" ++ show j | j <- [1..n]] | i <- [1..m] ]
	let enames = gennames "e"
	let anames = gennames "a"
	let bnames = gennames "b"
	let abnames = zipWith (zipWith (,)) anames bnames
	let patall names = conP name [conP (vectorName m) $ map (conP (vectorName n) . map varP) names]
	let expall rows = appE (conE name) $ foldl appE (conE $ vectorName m) $ map (foldl appE (conE (vectorName n))) rows
	-- newtype MatrixMxN e = MatrixMxN (VectorM (VectorN e))
	nt <- newtypeD (cxt []) name [PlainTV e] (normalC name [strictType notStrict $ appT (conT $ vectorName m) $ appT (conT $ vectorName n) et]) [''Eq, ''Typeable, ''Data]
	inst <- instanceD (cxt []) (foldl' appT (conT ''Matrix) [conT name, conT $ vectorName n, conT $ vectorName m])
		[ funD 'matrixToRows [clause [conP name [varP $ mkName "m"]] (normalB $ varE $ mkName "m") [] ]
		, funD 'matrixFromRows [clause [] (normalB $ conE name) []]
		, funD 'matrixToCols [clause [patall enames] (normalB $ foldl appE (conE $ vectorName n) $ map (foldl appE (conE $ vectorName m) . map varE) $ transpose enames) []]
		, funD 'matrixFromCols [ clause [conP (vectorName n) $ map (conP (vectorName m) . map varP) $ transpose enames] (normalB $ expall $ map (map varE) enames) [] ]
		]
	foldableinst <- instanceD (cxt []) (appT (conT ''F.Foldable) $ conT name)
		[ funD 'foldr [clause [] (normalB [e|\f z v -> F.foldr (\r z' -> F.foldr f z' r) z $ matrixToRows v |]) [] ] ]
	functorinst <- instanceD (cxt []) (appT (conT ''Functor) $ conT name)
		[ funD 'fmap [clause [varP $ mkName "f", conP name [varP $ mkName "m"]] (normalB $ appE (conE name) $ foldl appE (varE 'fmap) [appE (varE 'fmap) $ varE $ mkName "f", varE $ mkName "m"]) [] ]
		]
	applicativeinst <- instanceD (cxt []) (appT (conT ''Applicative) $ conT name)
		[ funD 'pure [clause [] (normalB [e| $(conE name) . pure . pure |]) [] ]
		, funD '(<*>) [clause [] (normalB [e|\a b -> $(conE name) $ fmap (<*>) (matrixToRows a) <*> (matrixToRows b) |]) [] ]
		]
	traversableinst <- instanceD (cxt []) (appT (conT ''Traversable) $ conT name)
		[ funD 'traverse [clause [] (normalB [e|\f -> fmap matrixFromRows . traverse (traverse f) . matrixToRows |]) []]
		]
	numinst <- instanceD (cxt [classP ''Num [et]]) (appT (conT ''Num) $ appT (conT name) et)
		[ funD '(+) [ clause [patall anames, patall bnames]
			(normalB $ expall $ map (map (\(a, b) -> foldl appE (varE '(+)) [varE a, varE b])) abnames) [] ]
		, funD '(-) [ clause [patall anames, patall bnames]
			(normalB $ expall $ map (map (\(a, b) -> foldl appE (varE '(-)) [varE a, varE b])) abnames) [] ]
		, funD 'negate [ clause [] (normalB $ appE (varE 'fmap) (varE 'negate)) [] ]
		, if n == m
			then funD '(*) [ clause [] (normalB [e| matrixProduct |]) [] ]
			else funD '(*) [ clause [] (normalB [e| error "rows/cols are not the same size" |]) [] ]
		, funD 'abs [ clause [] (normalB [e| id |]) [] ]
		, funD 'signum [ clause [] (normalB $ appE (varE 'const) $ litE $ integerL 1) [] ]
		, funD 'fromInteger [ clause [varP $ mkName "e'"] (normalB $ expall $ [ [ if i == j then varE e else litE $ integerL 0 | j <- [1..n] ] | i <- [1..m] ] )
			[valD (varP e) (normalB $ appE (varE 'fromInteger) (varE $ mkName "e'")) []] ]
		]
	nfdatainst <- instanceD (cxt [classP ''NFData [et]]) (appT (conT ''NFData) $ appT (conT name) et)
		[ funD 'rnf [clause [conP name [varP $ mkName "m"]] (normalB $ appE (varE 'rnf) $ varE $ mkName "m") []]
		]
	showinst <- instanceD (cxt [classP ''Show [et]]) (appT (conT ''Show) $ appT (conT name) et)
		[ funD 'show [clause [] (normalB $ [e|show . toAnyMatrix|]) []]
		]
	multinst1 <- instanceD (cxt []) (foldl appT (conT ''Mult) [conT name, conT $ vectorName n, conT $ vectorName m])
		[ funD '(~*) [clause [] (normalB [e| matrixMultVector |]) []]
		]
	multinst2 <- instanceD (cxt []) (foldl appT (conT ''Mult) [conT $ vectorName m, conT name, conT $ vectorName n])
		[ funD '(~*) [clause [] (normalB [e| vectorMultMatrix |]) []]
		]
	multinst3 <- if n == m then declareMatrixMult n n n else return []
	sigconstr <- sigD (matrixname m n) $ forallT [PlainTV e] (cxt []) $ foldl appT arrowT [foldl appT (tupleT m) $ replicate m (foldl appT (tupleT n) $ replicate n et), appT (conT name) et]
	constr <- funD (matrixname m n) [clause [tupP $ map varP rownames] (normalB $ appE (conE name) $ foldl appE (conE $ vectorName m) $ map (appE (varE $ vectorname n) . varE) rownames) []]
--	trace (pprint nt) $
	return $ [nt,inst,foldableinst,functorinst,applicativeinst,traversableinst,numinst,nfdatainst,showinst,multinst1,multinst2,sigconstr,constr] ++ multinst3

{-
instance Mult MatrixMxK MatrixKxN MatrixMxN where ~* = matrixProduct
-}
declareMatrixMult :: Int {- ^ m -} -> Int {- ^ k -} -> Int {- ^ n -} -> Q [Dec]
declareMatrixMult m k n = do
	i <- instanceD (cxt []) (foldl appT (conT ''Mult) [conT $ matrixName m k, conT $ matrixName k n, conT $ matrixName m n])
		[ funD '(~*) [clause [] (normalB [e| matrixProduct |]) []] ]
	return [i]
