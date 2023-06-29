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

module Types.Stack where

import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           GHC.Generics               (Generic)
import           Prelude                    

import           Types.PolyData             (PolyDataItem, ToPolyData (..), Index, findIndex)
import           Types.Term                 (Term, TermArray)
import           Types.Value                (Value, Environment, ValueArray, EnvironmentArray)

--------------------------------- Frame ---------------------------------------

data Frame =
    ForceF
  | ApplyLeftF Term Environment
  | ApplyRightF Value
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToPolyData (TermArray, ValueArray, EnvironmentArray) Frame (PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData _            ForceF          = (0, 0, 0)
    toPolyData (ts, _, es) (ApplyLeftF t e) = (1, findIndex t ts, findIndex e es)
    toPolyData (_, vs, _)  (ApplyRightF v)  = (2, findIndex v vs, 0)

--------------------------------- Stack ---------------------------------------

type Stack = [Frame]

instance ToPolyData (TermArray, ValueArray, EnvironmentArray, StackArray) Stack (PolyDataItem, PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData _ [] = (-1, -1, -1, -1)
    toPolyData (ts, vs, es, ss) (sh : s') =
        let (sig1, sig2, sig3) = toPolyData (ts, vs, es) sh
        in (sig1, sig2, sig3, findIndex s' ss)

--------------------------------- StackIndexed ---------------------------------

type StackIndexed = (Stack, Index)

instance ToPolyData (TermArray, ValueArray, EnvironmentArray, StackArray) StackIndexed (PolyDataItem, PolyDataItem, PolyDataItem, PolyDataItem) where
    toPolyData as (s, _) = toPolyData as s

--------------------------------- StackArray ---------------------------------

type StackArray = [StackIndexed]

instance ToPolyData (TermArray, ValueArray, EnvironmentArray, StackArray) StackArray [(PolyDataItem, PolyDataItem, PolyDataItem, PolyDataItem)] where
    toPolyData as = map (toPolyData as)