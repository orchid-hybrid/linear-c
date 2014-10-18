module Typecheck (
 check
 ) where

import Control.Monad.State
import Control.Monad.Trans.Maybe

import Linear

qTest :: Q -> Context -> Bool
qTest q Empty = True
qTest q (gamma ::: (_,(q',_))) = q <= q' && qTest q gamma

qTest' q (q',_) = q <= q'

contextLookup x gamma = contextLookup' Empty x gamma where
 contextLookup' acc x Empty = mzero
 contextLookup' acc x (gamma ::: (x',ty)) | x == x' = return (acc, ty, gamma)
 contextLookup' acc x (gamma ::: b) = contextLookup' (acc ::: b) x gamma



check :: T -> MaybeT (State Context) Ty

-- ========================================================= (A-UVar)
-- Gamma1, x:un P, Gamma2 |- x:un P ; Gamma1, x:un P, Gamma2
--
-- =================================================== (A-LVar)
-- Gamma1, x:lin P, Gamma2 |- x:lin P ; Gamma1, Gamma2
check (Var x) = do
 gamma <- get
 (gamma1, ty@(q, t), gamma2) <- contextLookup x gamma
 case q of
  Un -> do return ty
  Lin -> do put (gamma1 `append` gamma2)
            return ty

check (Boole q b) = return (q, BooleTy)
-- Gamma1 |- t1 : q Bool ; Gamma2
-- Gamma2 |- t2 : T ; Gamma3
-- Gamma2 |- t3 : T ; Gamma3
-- ============================================ (A-If)
-- Gamma1 |- If t1 Then t2 Else t3 : T ; Gamma3
check (IfThenElse b t e) = do
 (bq,bty) <- check b
 guard (bty == BooleTy)
 gamma2 <- get
 (tq,tty) <- check t
 gamma3 <- get
 put gamma2
 (eq,ety) <- check e
 gamma3' <- get
 guard (tty == ety)
 guard (gamma3 == gamma3')
 return (bq, BooleTy)

-- Gamma1 |- t1 : T1 ; Gamma2
-- Gamma2 |- t2 : T2 ; Gamma3
-- q(T1)
-- q(T2)
-- =========================================== (A-Pair)
-- Gamma1 |- q <t1, t2> : q (T1 * T2) ; Gamma3
check (Pair q t1 t2) = do
 t1Ty <- check t1
 t2Ty <- check t2
 guard (qTest' q t1Ty)
 guard (qTest' q t2Ty)
 return (q, (t1Ty :* t2Ty))

-- Gamma1 |- t1 : T11 -> T12 ; Gamma2
-- Gamma2 |- t2 : T11 ; Gamma3
-- ============================== (A-App)
-- Gamma1 |- t1 t2 : T12 ; Gamma3
check (t1 :$ t2) = do
 (_, t11 :-> t12) <- check t1
 t11' <- check t2
 guard (t11 == t11')
 return t12

runCheck t = runState (runMaybeT (check t))
