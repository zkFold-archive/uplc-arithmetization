{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module CEK where

import           Data.Bool                   (bool)
import           PlutusTx.Prelude            ((!!))
import           Prelude                     hiding ((!!), drop, take)
import           Text.Pretty.Simple          (pPrint)

import           Eval                        (evalCEK)
import           Types.Phase                 (PhaseArray, Phase (..))
import           Types.PolyData              (Rule, Rules, alpha, omega)
import           Types.Stack                 (Frame (..), StackArray)
import           Types.State                 (State(..), StateArray)
import           Types.Term                  (Term (..), TermArray)
import           Types.Value                 (Value (..), ValueArray, EnvironmentArray)

-------------------------- CEK Machine execution ------------------------------

cekInit :: Term -> State
cekInit = Compute [] []

cekStep :: State -> (State, Rule)
cekStep (Compute s ((x', v) : e') t@(Var x)) = (bool (Compute s e' t) (Return s v) (x == x'), 1)
cekStep (Compute s _ (Con c)) = (Return s (ConV c), 2)
cekStep (Compute s e (Lam x t)) = (Return s (LamV x t e), 3)
cekStep (Compute s e (Delay t)) = (Return s (DelayV t e), 4)
cekStep (Compute s e (Force t)) = (Compute (ForceF : s) e t, 5)
cekStep (Compute s e (Apply t1 t2)) = (Compute (ApplyLeftF t2 e : s) e t1, 6)
cekStep (Compute s _ (Builtin b)) = (Return s (BuiltinV b [] (alpha !! b)), 7)
cekStep (Compute _ _ Error) = (ErrorState, 8)
cekStep (Return [] v) = (Halt v, 9)
cekStep (Return (ApplyLeftF t e : s) v) = (Compute (ApplyRightF v : s) e t, 10)
cekStep (Return (ApplyRightF (LamV x t e') : s) v) = (Compute s ((x, v) : e') t, 11)
cekStep (Return (ForceF : s) (DelayV t e)) = (Compute s e t, 12)
cekStep (Return (ForceF : s) (BuiltinV b vs [i])) =
    (bool ErrorState (evalCEK s b vs) (i `elem` omega), 14)
cekStep (Return (ForceF : s) (BuiltinV b vs (i:is))) =
    (bool ErrorState (Return s (BuiltinV b vs is)) (i `elem` omega), 13)
cekStep (Return (ApplyRightF (BuiltinV b vs [i]) : s) v) =
    (bool ErrorState (evalCEK s b (vs ++ [v])) (i `notElem` omega), 16)
cekStep (Return (ApplyRightF (BuiltinV b vs (i:is)) : s) v) =
    (bool ErrorState (Return s (BuiltinV b (vs ++ [v]) is)) (i `notElem` omega), 15)
cekStep _ = (ErrorState, 0)

-------------------------- CEK Machine trace ------------------------------

cekTrace' :: State -> [State]
cekTrace' s = s : case fst (cekStep s) of
    Halt v     -> [Halt v]
    ErrorState -> [ErrorState]
    s'         -> cekTrace' s'

cekTrace :: State -> StateArray
cekTrace s = zip (cekTrace' s) [0..]

cekRules :: State -> Rules
cekRules s =
    let (s', r) = cekStep s
    in r : case r of
        0 -> []
        8 -> []
        9 -> []
        _ -> cekRules s'

cekTracePrint :: State -> IO ()
cekTracePrint = mapM_ pPrint . cekTrace

class FromTrace a where
    fromTrace :: StateArray -> a

instance FromTrace StackArray where
    fromTrace ((Compute s _ _, i) : ss) = (s, i) : fromTrace ss
    fromTrace ((Return s _, i) : ss)    = (s, i) : fromTrace ss
    fromTrace ((Halt _, i): _)          = [([], i)]
    fromTrace ((ErrorState, i) : _)     = [([], i)]
    fromTrace []                        = []

instance FromTrace EnvironmentArray where
    fromTrace ((Compute _ e _, i) : ss) = (e, i) : fromTrace ss
    fromTrace ((Return _ _, i) : ss)    = ([], i) : fromTrace ss
    fromTrace ((Halt _, i) : _)         = [([], i)]
    fromTrace ((ErrorState, i) : _)     = [([], i)]
    fromTrace _                         = []

instance FromTrace TermArray where
    fromTrace ((Compute _ _ t, i) : ss) = (t, i) : fromTrace ss
    fromTrace ((Return _ _, i) : ss)    = (Error, i) : fromTrace ss
    fromTrace ((Halt _, i): _)          = [(Error, i)]
    fromTrace ((ErrorState, i) : _)     = [(Error, i)]
    fromTrace _                         = []

instance FromTrace ValueArray where
    fromTrace ((Compute {}, i) : ss) = (ErrorV, i) : fromTrace ss
    fromTrace ((Return _ v, i) : ss) = (v, i) : fromTrace ss
    fromTrace ((Halt v, i): _)       = [(v, i)]
    fromTrace ((ErrorState, i) : _)  = [(ErrorV, i)]
    fromTrace _                      = []

instance FromTrace PhaseArray where
    fromTrace ((Compute {}, i) : ss) = (ComputePhase, i) : fromTrace ss
    fromTrace ((Return _ _, i) : ss) = (ReturnPhase, i) : fromTrace ss
    fromTrace ((Halt _, i): _)       = [(ReturnPhase, i)]
    fromTrace ((ErrorState, i) : _)  = [(ReturnPhase, i)]
    fromTrace _                      = []