{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Types.Term where

import           Data.Aeson              (FromJSON (..), ToJSON (..))
import           Data.Bool               (bool)
import           GHC.Generics            (Generic)
import           Prelude                 hiding ((!!), drop, take, length)
import           PlutusTx.Prelude        ((!!), length)

import           Types.Constant          (Constant, ConstantArray)
import           Types.PolyData          (Index, PolyDataItem, ToPolyData (..), Variable, findIndex)

--------------------------------- Term ---------------------------------------

data Term =
      Var     Variable
    | Con     Constant
    | Lam     Variable Term
    | Delay   Term
    | Force   Term
    | Apply   Term Term
    | Builtin Index
    | Error
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToPolyData (ConstantArray, TermArray) Term (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData _       (Var i)       = (0, i, 0)
    toPolyData (cs, _) (Con c)       = (1, findIndex c cs, 0)
    toPolyData (_, ts) (Lam i t)     = (2, i, findIndex t ts)
    toPolyData (_, ts) (Delay t)     = (3, findIndex t ts, 0)
    toPolyData (_, ts) (Force t)     = (4, findIndex t ts, 0)
    toPolyData (_, ts) (Apply t1 t2) = (5, findIndex t1 ts, findIndex t2 ts)
    toPolyData _       (Builtin b)   = (6, b, 0)
    toPolyData _       Error         = (7, 0, 0)

--------------------------------- TermIndexed ---------------------------------

type TermIndexed = (Term, Index)

instance ToPolyData (ConstantArray, TermArray) TermIndexed (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData ts (t, _) = toPolyData ts t

loadTerm :: ConstantArray -> [(PolyDataItem, PolyDataItem, PolyDataItem)] -> Index -> Term
loadTerm cs ts i
    | i >= length ts = Error
    | otherwise      =
        let (tType, tArg1, tArg2) = ts !! i
        in case tType of
            0 -> Var tArg1
            1 -> bool Error (Con (fst $ cs !! tArg1)) (tArg1 < length cs)
            2 -> Lam tArg1 (loadTerm cs ts tArg2)
            3 -> Delay (loadTerm cs ts tArg1)
            4 -> Force (loadTerm cs ts tArg1)
            5 -> Apply (loadTerm cs ts tArg1) (loadTerm cs ts tArg2)
            6 -> Builtin tArg1
            7 -> Error
            _ -> Error

--------------------------------- TermArray ---------------------------------

type TermArray = [TermIndexed]

instance ToPolyData (ConstantArray, TermArray) TermArray [(PolyDataItem, PolyDataItem, PolyDataItem)] where
    toPolyData ts = map (toPolyData ts)

loadTermArray :: ConstantArray -> [(PolyDataItem, PolyDataItem, PolyDataItem)] -> TermArray
loadTermArray cs ts =
    let m = length ts
    in map (\i -> (loadTerm cs ts i, i)) [0..m-1]