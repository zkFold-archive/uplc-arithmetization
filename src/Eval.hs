{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Eval where

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
-- eval 10 [vX, vY]
--     | ConV (CBytes x) <- vX, ConV (CBytes y) <- vY = ConV (CBytes $ toPolyData () $ appendByteString x y)
eval _ _ = ErrorV