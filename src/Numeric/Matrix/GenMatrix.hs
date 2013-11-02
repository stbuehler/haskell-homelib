{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Numeric.Matrix.GenMatrix
	( defineVector
	, defineMatrix
	, manyDecs
	) where

import Numeric.Matrix.Classes

import Language.Haskell.TH
import Data.Tagged
import Data.List

manyDecs :: [Q [Dec]] -> Q [Dec]
manyDecs = fmap concat . sequence

-- import Debug.Trace

vectorName :: Int -> Name
vectorName n = mkName $ "Vector" ++ show n

defineVector :: Int -> Q [Dec]
defineVector n = do
	let name = vectorName n
	let e = mkName "e"
	let et = varT e
	let tupt = foldl' appT (tupleT n) $ replicate n et
	let enames = map (\i -> mkName $ "e" ++ show i) [1..n]
	nt <- newtypeD (cxt []) name [PlainTV e] (normalC name [strictType notStrict tupt]) [''Eq]
	inst <- instanceD (cxt []) (appT (appT (conT ''Vector) $ appT (conT name) et) et)
		[ funD 'vectorToList [clause [conP name [tupP $ map varP enames]] (normalB $ listE $ map varE enames) []]
		, funD 'vectorFromList
			[ clause [listP $ map varP enames] (normalB $ appE (conE name) $ tupE $ map varE enames) []
			, clause [wildP] (normalB $ [e| error "list length doesn't match vector size" |]) []
			]
		, funD 'vectorSize [clause [] (normalB [e|Tagged n|]) []]
		]
	showinst <- [d|instance Show e => Show ($(conT name) e) where show = show . rowToAnyMatrix|]
	-- trace (pprint [nt, inst])
	return $ [nt,inst] ++ showinst

matrixName :: Int -> Int -> Name
matrixName m n = mkName $ "Matrix" ++ show m ++ "x" ++ show n

defineMatrix :: Int -> Int -> Q [Dec]
defineMatrix m n = do
	-- m rows, n cols. a row vector has n entries, a col vector m.
	let name = matrixName m n
	let e = mkName "e"
	let et = varT e
	let tupt = foldl' appT (tupleT m) $ replicate m $ foldl' appT (tupleT n) $ replicate n et
	let rownames = map (\r -> mkName $ "r" ++ show r) [1..m]
	let enames = [ [ mkName $ "e" ++ show i ++ "_" ++ show j | j <- [1..n]] | i <- [1..m] ]
	let anames = [ [ mkName $ "a" ++ show i ++ "_" ++ show j | j <- [1..n]] | i <- [1..m] ]
	let bnames = [ [ mkName $ "b" ++ show i ++ "_" ++ show j | j <- [1..n]] | i <- [1..m] ]
	let abnames = zipWith (zipWith (,)) anames bnames
	nt <- newtypeD (cxt []) name [PlainTV e] (normalC name [strictType notStrict tupt]) [''Eq]
	inst <- instanceD (cxt []) (foldl' appT (conT ''Matrix) [appT (conT name) et, appT (conT $ vectorName n) et, appT (conT $ vectorName m) et, et])
		[ funD 'toRowVectors [clause [conP name [tupP $ map varP rownames]] (normalB $ listE $ map (appE (conE $ vectorName n) . varE) rownames) []]
		, funD 'toColVectors [clause [conP name [tupP $ map (tupP . map varP) enames]] (normalB $ listE $ map (appE (conE $ vectorName m) . tupE . map varE) $ transpose enames) []]
		, funD 'fromRowVectors
			[ clause [listP $ map (\r -> conP (vectorName n) [varP r]) rownames] (normalB $ appE (conE name) $ tupE $ map varE rownames) []
			, clause [wildP] (normalB $ [e| error "rows number doesn't match matrix size" |]) []
			]
		, funD 'fromColVectors
			[ clause [listP $ map (\c -> conP (vectorName m) [tupP $ map varP c]) $ transpose enames] (normalB $ appE (conE name) $ tupE $ map (tupE . map varE) enames) []
			, clause [wildP] (normalB $ [e| error "rows number doesn't match matrix size" |]) []
			]
		, funD 'matrixDimensions [clause [] (normalB [e|Tagged (m, n)|]) []]
		]
	numinst <- instanceD (cxt [classP ''Num [et]]) (appT (conT ''Num) $ appT (conT name) et)
		[ funD '(+) [ clause [conP name [tupP $ map (tupP . map varP) anames], conP name [tupP $ map (tupP . map varP) bnames]]
			(normalB $ appE (conE name) $ tupE $ map (tupE . map (\(a, b) -> appE (appE (varE '(+)) $ varE a) $ varE b)) abnames) [] ]
		, funD '(-) [ clause [conP name [tupP $ map (tupP . map varP) anames], conP name [tupP $ map (tupP . map varP) bnames]]
			(normalB $ appE (conE name) $ tupE $ map (tupE . map (\(a, b) -> appE (appE (varE '(-)) $ varE a) $ varE b)) abnames) [] ]
		, if n == m
			then funD '(*) [ clause [] (normalB [e| matrixProduct |]) [] ]
			else funD '(*) [ clause [] (normalB [e| error "rows/cols are not the same size" |]) [] ]
		, funD 'abs [ clause [] (normalB [e| id |]) [] ]
		, funD 'signum [ clause [] (normalB $ appE (varE 'const) $ litE $ integerL 1) [] ]
		, funD 'fromInteger [ clause [varP $ mkName "e'"] (normalB $ appE (conE name) $ tupE [ tupE [ if i == j then varE e else litE $ integerL 0 | j <- [1..n] ] | i <- [1..m] ]  )
			[valD (varP e) (normalB $ appE (varE 'fromInteger) (varE $ mkName "e'")) []] ]
		]
	showinst <- [d|instance Show e => Show ($(conT name) e) where show = show . toAnyMatrix|]
	-- trace (pprint numinst) $
	return $ [nt,inst,numinst] ++ showinst
