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

module Types.Constant where

import           Data.Aeson              (FromJSON (..), ToJSON (..))
import           GHC.Generics            (Generic)
import           Prelude                 hiding ((!!), length)
import           PlutusTx.Prelude        ((!!), length)

import           Types.PolyData          (Index, PolyDataItem, ToPolyData (..), Byte, findIndex)

--------------------------------- Constant ---------------------------------------

data Constant =
      CInt   PolyDataItem
    | CBytes (Byte, Constant)
    | CString (PolyDataItem, Constant)
    | CUnit
    | CBool PolyDataItem
    | CList  (Constant, Constant)
    | CPair  (Constant, Constant)
    | CApply
    | CData Constant
    | CError
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToPolyData ConstantArray Constant (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData _  (CInt i)          = (0, i, 0)
    toPolyData cs (CBytes (b, bs))  = (1, b, findIndex bs cs)
    toPolyData cs (CString (c, s))  = (2, c, findIndex s cs)
    toPolyData _  CUnit             = (3, 0, 0)
    toPolyData _  (CBool b)         = (4, b, 0)
    toPolyData cs (CList  (a, as))  = (5, findIndex a cs, findIndex as cs)
    toPolyData cs (CPair (l, r))    = (6, findIndex l cs, findIndex r cs)
    toPolyData _  CApply            = (7, 0, 0)
    toPolyData cs (CData d)         = (8, findIndex d cs, 0)
    toPolyData _  CError            = (9, 0, 0)

--------------------------------- ConstantIndexed ---------------------------------

type ConstantIndexed = (Constant, Index)

instance ToPolyData ConstantArray ConstantIndexed (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData cs (t, _) = toPolyData cs t

loadConstant :: [(PolyDataItem, PolyDataItem, PolyDataItem)] -> Index -> Constant
loadConstant ts i
    | i >= length ts = CError
    | otherwise      =
        let (tType, tArg1, tArg2) = ts !! i
        in case tType of
            0 -> CInt tArg1
            1 -> CBytes (tArg1, loadConstant ts tArg2)
            2 -> CList (loadConstant ts tArg1, loadConstant ts tArg2)
            3 -> CPair (loadConstant ts tArg1, loadConstant ts tArg2)
            _ -> CError

--------------------------------- ConstantArray ----------------------------------

type ConstantArray = [ConstantIndexed]

instance ToPolyData ConstantArray ConstantArray [(PolyDataItem, PolyDataItem, PolyDataItem)] where
    toPolyData cs = map (toPolyData cs)

loadConstantArray :: [(PolyDataItem, PolyDataItem, PolyDataItem)] -> ConstantArray
loadConstantArray ts =
    let m = length ts
    in map (\i -> (loadConstant ts i, i)) [0..m-1]