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
{-# LANGUAGE LambdaCase #-}

module Types.Value where

import           Data.Aeson               (FromJSON (..), ToJSON (..))
import           GHC.Generics             (Generic)
import           Prelude                  

import           Types.Constant           (Constant, ConstantArray)
import           Types.PolyData           (Index, PolyDataItem, ToPolyData (..), Variable, findIndex, ExpectedArgumentsArray)
import           Types.Term               (Term, TermArray)

--------------------------------------- Value ---------------------------------------------

data Value =
      ConV     Constant
    | DelayV   Term     Environment
    | LamV     Variable Term        Environment
    | BuiltinV Index    [Value]     [Integer]
    | ErrorV
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToPolyData (ExpectedArgumentsArray, ConstantArray, TermArray, EnvironmentArray, ValueListArray) Value (PolyDataItem, PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData (_, cs, _,  _, _)   (ConV c)           = (0, findIndex c cs, 0, 0)
    toPolyData (_, _, ts, es, _)   (DelayV t e)       = (1, findIndex t ts, findIndex e es, 0)
    toPolyData (_, _, ts, es, _)   (LamV i t e)       = (2, i, findIndex t ts, findIndex e es)
    toPolyData (eas, _, _, _, vls) (BuiltinV i vl ea) = (3, i, findIndex vl vls , findIndex ea eas)
    toPolyData _           ErrorV               = (4, 0, 0, 0)

getBuiltinArguments :: Value -> [Value]
getBuiltinArguments = \case
    BuiltinV _ vl _ -> vl
    _               -> []

--------------------------------------- ValueIndexed --------------------------------------

type ValueIndexed = (Value, Index)

instance ToPolyData (ExpectedArgumentsArray, ConstantArray, TermArray, EnvironmentArray, ValueListArray) ValueIndexed (PolyDataItem, PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData as (v, _) = toPolyData as v

--------------------------------------- ValueArray ----------------------------------------

type ValueArray = [ValueIndexed]

instance ToPolyData (ExpectedArgumentsArray, ConstantArray, TermArray, EnvironmentArray, ValueListArray) ValueArray [(PolyDataItem, PolyDataItem, PolyDataItem, PolyDataItem)] where
    toPolyData as = map (toPolyData as)

--------------------------------------- ValueList -----------------------------------------

type ValueList = [Value]

instance ToPolyData (ValueArray, ValueListArray) ValueList (PolyDataItem, PolyDataItem) where
    toPolyData _ []         = (-1, -1)
    toPolyData (vs, vls) vl = (findIndex (head vl) vs, findIndex (tail vl) vls)

--------------------------------------- ValueListIndexed ----------------------------------

type ValueListIndexed = (ValueList, Index)

instance ToPolyData (ValueArray, ValueListArray) ValueListIndexed (PolyDataItem, PolyDataItem) where
    toPolyData (vs, vls) (vl, _) = toPolyData (vs, vls) vl

--------------------------------------- ValueListArray ------------------------------------

type ValueListArray = [ValueListIndexed]

instance ToPolyData (ValueArray, ValueListArray) ValueListArray [(PolyDataItem, PolyDataItem)] where
    toPolyData (vs, vls) = map (toPolyData (vs, vls))

getBuiltinArgumentsArray :: ValueArray -> ValueListArray
getBuiltinArgumentsArray = flip zip [0..] . map (\(v, _) -> getBuiltinArguments v)

--------------------------------------- Environment ----------------------------------------

type Environment = [(Variable, Value)]

instance ToPolyData (ValueArray, EnvironmentArray) Environment (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData _ []                   = (-1, -1, -1)
    toPolyData (va, ea) ((i, v) : e') = (i, findIndex v va, findIndex e' ea)

--------------------------------------- EnvironmentIndexed ---------------------------------

type EnvironmentIndexed = (Environment, Index)

instance ToPolyData (ValueArray, EnvironmentArray) EnvironmentIndexed (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData (va, ea) (e, _) = toPolyData (va, ea) e

--------------------------------------- EnvironmentArray -----------------------------------

type EnvironmentArray = [EnvironmentIndexed]

instance ToPolyData (ValueArray, EnvironmentArray) EnvironmentArray [(PolyDataItem, PolyDataItem, PolyDataItem)] where
    toPolyData (va, ea) = map (toPolyData (va, ea))