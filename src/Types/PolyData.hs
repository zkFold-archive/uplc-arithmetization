{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Types.PolyData where

import Data.Bool (bool)
import Data.List (find)

--------------------------------- PolyDataItem --------------------------------------

type PolyDataItem = Integer
type PolyData = [PolyDataItem]

class ToPolyData d a b where
    toPolyData :: d -> a -> b

instance ToPolyData () Bool PolyDataItem where
    toPolyData () = bool 0 1

boundedPolyDataItem :: Integer -> PolyDataItem -> PolyDataItem
boundedPolyDataItem n x
  | x < 0  = 0
  | x >= n = n - 1
  | otherwise = x

--------------------------------- Index ---------------------------------------

type Index = PolyDataItem

findIndex :: Eq a => a -> [(a, Index)] -> Index
findIndex _ [] = -1
findIndex x ys = maybe (-1) snd (find (\(x', _) -> x == x') ys)

--------------------------------- Tail ----------------------------------------

type Tail = Index

--------------------------------- Variable ------------------------------------

type Variable = Index

--------------------------------- Byte ----------------------------------------

type Byte = PolyDataItem

--------------------------------- Rule -----------------------------------------

type Rule = PolyDataItem
type Rules = [Rule]

---------------------------- Expected argument ---------------------------------

type ExpectedArgument = PolyDataItem
type ExpectedArguments = [ExpectedArgument]

omega :: ExpectedArguments
omega = [0]

type ExpectedArgumentsIndexed = (ExpectedArguments, Index)

type ExpectedArgumentsArray = [ExpectedArgumentsIndexed]

builtinOperationsSignaturesArray :: ExpectedArgumentsArray
builtinOperationsSignaturesArray = [
        ([1], 0),
        ([1, 1], 1),
        ([1, 1, 1], 2),
        ([0, 1], 3),
        ([0, 1, 1], 4),
        ([0, 1, 1, 1], 5),
        ([0, 1, 1, 1, 1], 6),
        ([0, 1, 1, 1, 1, 1], 7),
        ([0, 1, 1, 1, 1, 1, 1], 8),
        ([0, 0, 1], 9),
        ([0, 0, 1, 1], 10),
        ([0, 0, 1, 1, 1], 11)
    ]

alpha :: [ExpectedArguments]
alpha = [
        [1, 1], -- 0
        [1, 1], -- 1
        [1, 1], -- 2
        [1, 1], -- 3
        [1, 1], -- 4
        [1, 1], -- 5
        [1, 1], -- 6
        [1, 1], -- 7
        [1, 1], -- 8
        [1, 1], -- 9
        [], -- 10
        [1, 1], -- 11
        [], -- 12
        [], -- 13
        [], -- 14
        [1, 1], -- 15
        [], -- 16
        [], -- 17
        [], -- 18
        [], -- 19
        [], -- 20
        [], -- 21
        [], -- 22
        [], -- 23
        [], -- 24
        [], -- 25
        [0, 1, 1, 1], -- 26
        [], -- 27
        [], -- 28
        [0, 0, 1], -- 29
        [0, 0, 1], -- 30
        [], -- 31
        [0, 1, 1], -- 32
        [0, 1], -- 33
        [0, 1], -- 34
        [0, 1] -- 35
    ]