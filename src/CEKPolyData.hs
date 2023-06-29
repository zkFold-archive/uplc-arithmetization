{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module CEKPolyData where

import           Data.Aeson                    (ToJSON, FromJSON)
import           Data.List                     (unzip4)
import           GHC.Generics                  (Generic)
import           Prelude                       hiding ((!!), length)
import           PlutusTx.Prelude              ((!!), length)

import           CEK                           (FromTrace(..), cekTrace, cekInit, cekRules)
import           Types.Constant                (ConstantArray)
import           Types.Phase                   (PhaseArray)
import           Types.PolyData                (PolyDataItem, PolyData, ToPolyData (..), Rules, boundedPolyDataItem, builtinOperationsSignaturesArray)
import           Types.Stack                   (StackArray)
import           Types.Term                    (Term (..), TermArray)
import           Types.Value                   (ValueArray, EnvironmentArray, getBuiltinArgumentsArray)

data CEKPolyData = CEKPolyData
    {
        stackHeadConstructor :: PolyData,
        stackHeadArgument1   :: PolyData,
        stackHeadArgument2   :: PolyData,
        stackTail            :: PolyData,

        stackHeadConstructorPrev :: PolyData,
        stackHeadArgument1Prev :: PolyData,
        stackHeadArgument2Prev :: PolyData,
        stackTailPrev :: PolyData,

        envId :: PolyData,
        envX :: PolyData,
        envV :: PolyData,
        envTail :: PolyData,

        envIdPrev :: PolyData,
        envXPrev :: PolyData,
        envVPrev :: PolyData,
        envTailPrev :: PolyData,

        triangle :: PolyData,

        termId          :: PolyData,
        termConstructor :: PolyData,
        termArgument1   :: PolyData,
        termArgument2   :: PolyData,

        valueConstructor :: PolyData,
        valueArgument1   :: PolyData,
        valueArgument2   :: PolyData,
        valueArgument3   :: PolyData,

        valueConstructorInStack :: PolyData,
        valueArgument1InStack   :: PolyData,
        valueArgument2InStack   :: PolyData,
        valueArgument3InStack   :: PolyData,

        valueConstructorInEnv :: PolyData,
        valueArgument1InEnv   :: PolyData,
        valueArgument2InEnv   :: PolyData,
        valueArgument3InEnv   :: PolyData,

        builtinOperationArgsHeadInValue :: PolyData,
        builtinOperationArgsTailInValue :: PolyData,

        rules :: Rules
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

cekPolyData :: ConstantArray -> Term -> CEKPolyData
cekPolyData cs t = CEKPolyData {..}
    where
        states = cekTrace $ cekInit t
        nStates = length states

        eas = builtinOperationsSignaturesArray

        ss = fromTrace states :: StackArray
        es = fromTrace states :: EnvironmentArray
        ts = fromTrace states :: TermArray
        vs = fromTrace states :: ValueArray

        vls = getBuiltinArgumentsArray vs

        stackPolyData = toPolyData (ts, vs, es, ss) ss
        (stackHeadConstructor, stackHeadArgument1, stackHeadArgument2, stackTail) = unzip4 stackPolyData

        stackPrev                 = map ((ss !!) . boundedPolyDataItem nStates) stackTail
        stackPrevPolyData         = toPolyData (ts, vs, es, ss) stackPrev
        (stackHeadConstructorPrev, stackHeadArgument1Prev, stackHeadArgument2Prev, stackTailPrev) = unzip4 stackPrevPolyData

        envId       = map snd es :: PolyData
        envPolyData = toPolyData (vs, es) es
        (envX, envV, envTail) = unzip3 envPolyData

        envPrev         = map ((es !!) . boundedPolyDataItem nStates) envTail
        envIdPrev       = map snd envPrev
        envPrevPolyData = toPolyData (vs, es) envPrev
        (envXPrev, envVPrev, envTailPrev) = unzip3 envPrevPolyData

        phases   = fromTrace states :: PhaseArray
        triangle = map (toPolyData ()) phases :: PolyData

        termId           = map snd ts :: PolyData
        termPolyData     = toPolyData (cs, ts) ts
        (termConstructor, termArgument1, termArgument2) = unzip3 termPolyData

        valuePolyData     = toPolyData (eas, cs, ts, es, vls) vs
        (valueConstructor, valueArgument1, valueArgument2, valueArgument3) = unzip4 valuePolyData

        valueInStack         = map ((vs !!) . boundedPolyDataItem nStates) stackHeadArgument1
        valueInStackPolyData = toPolyData (eas, cs, ts, es, vls) valueInStack
        (valueConstructorInStack, valueArgument1InStack, valueArgument2InStack, valueArgument3InStack) = unzip4 valueInStackPolyData

        valueInEnv         = map ((vs !!) . boundedPolyDataItem nStates) envV
        valueInEnvPolyData = toPolyData (eas, cs, ts, es, vls) valueInEnv
        (valueConstructorInEnv, valueArgument1InEnv, valueArgument2InEnv, valueArgument3InEnv) = unzip4 valueInEnvPolyData

        builtinOperationArgsHeadInValue = map fst (toPolyData (vs, vls) vls :: [(PolyDataItem, PolyDataItem)])
        builtinOperationArgsTailInValue = map snd (toPolyData (vs, vls) vls :: [(PolyDataItem, PolyDataItem)])

        rules = cekRules $ cekInit t