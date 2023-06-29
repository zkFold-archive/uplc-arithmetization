{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Eval where

import           Data.Bool                (bool)
import           PlutusTx.Builtins
import           Prelude

import           Types.Constant           (Constant (..))
import           Types.PolyData           (Index, ToPolyData (toPolyData))
import           Types.Stack              (Stack)
import           Types.State              (State (..))
import           Types.Value              (Value(..))

evalCEK :: Stack -> Index -> [Value] -> State
evalCEK s b vs = case eval b vs of
    ErrorV -> ErrorState
    v      -> Return s v

eval :: Index -> [Value] -> Value
eval 0 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ addInteger x y)
eval 1 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ subtractInteger x y)
eval 2 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ multiplyInteger x y)
eval 3 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ divideInteger x y)
eval 4 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ quotientInteger x y)
eval 5 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ remainderInteger x y)
eval 6 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CInt $ modInteger x y)
eval 7 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CBool $ toPolyData () $ equalsInteger x y)
eval 8 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CBool $ toPolyData () $ lessThanInteger x y)
eval 9 [vX, vY]
    | ConV (CInt x) <- vX, ConV (CInt y) <- vY = ConV (CBool $ toPolyData () $ lessThanEqualsInteger x y)
eval 11 [vX, vY]
    | ConV (CInt x) <- vX, ConV bs@(CBytes _) <- vY = ConV (CBytes (x, bs))
eval 15 [vX, vY]
    | ConV bs1@(CBytes _) <- vX, ConV bs2@(CBytes _) <- vY = ConV (CBool $ toPolyData () $ bs1 == bs2)
eval 26 [vX, vY, vZ]
    | ConV (CBool b) <- vX = bool vZ vY (b == 1)
eval 29 [vX]
    | ConV (CPair (c1, _)) <- vX = ConV c1
eval 30 [vX]
    | ConV (CPair (_, c2)) <- vX = ConV c2
eval 32 [vX, vY]
    | ConV x <- vX, ConV y@(CList _) <- vY = ConV (CList (x, y))
eval 33 [vX]
    | ConV (CList (x, _)) <- vX = ConV x
eval 34 [vX]
    | ConV (CList (_, xs)) <- vX = ConV xs
eval 35 [vX]
    | ConV (CList (x, xs)) <- vX = ConV (CBool $ toPolyData () $ x == CInt (-1) && xs == CInt (-1))
eval _ _ = ErrorV