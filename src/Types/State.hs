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

module Types.State where

import           Data.Aeson                (FromJSON (..), ToJSON (..))
import           GHC.Generics              (Generic)
import           Prelude                   hiding ((!!), drop, take)

import           Types.PolyData            (Index)
import           Types.Stack               (Stack)
import           Types.Term                (Term)
import           Types.Value               (Value, Environment)

data State =
      Compute Stack Environment Term
    | Return Stack Value
    | Halt Value
    | ErrorState
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

type StateIndexed = (State, Index)

type StateArray = [StateIndexed]