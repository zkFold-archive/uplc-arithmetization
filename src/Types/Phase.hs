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

module Types.Phase where

import           Data.Aeson               (FromJSON (..), ToJSON (..))
import           GHC.Generics             (Generic)
import           Prelude                  

import           Types.PolyData           (Index, PolyDataItem, ToPolyData (..))

--------------------------------- Phase ---------------------------------------

data Phase = ComputePhase | ReturnPhase
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToPolyData () Phase PolyDataItem where
    toPolyData _ ComputePhase = 0
    toPolyData _ ReturnPhase  = 1

--------------------------------- PhaseIndexed ---------------------------------

type PhaseIndexed = (Phase, Index)

instance ToPolyData () PhaseIndexed PolyDataItem where
    toPolyData _ (p, _) = toPolyData () p

--------------------------------- PhaseArray -----------------------------------

type PhaseArray = [PhaseIndexed]

instance ToPolyData () PhaseArray [PolyDataItem] where
    toPolyData _ = map (toPolyData ())