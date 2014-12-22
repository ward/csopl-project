module Main where

import Parser
import qualified Data.Map as Map
import Debug.Trace

type Env = Map.Map String (Either Type Kind)

main = do
    content <- getContents
    let parsed = calc . lexer $ content
    putStr "Input:     "
    putStr content
    putStr "Parsed:    "
    print parsed
    putStr "Type:      "
    print . findType $ parsed
    -- putStr "Evaluated: "
    -- print . eval $ parsed

findType :: Exp -> Type
findType exp = getType Map.empty exp

getType :: Env -> Exp -> Type
-- T-True
getType _ Btrue = Tbool
-- T-False
getType _ Bfalse = Tbool
-- T-If
getType env (If c t f)
    | getType env c == Tbool && getType env t == getType env f = getType env t
    | otherwise = error "Failed to get type @ if"
-- T-Zero
getType _ Zero = Tint
-- T-Succ
getType env (Succ e)
    | getType env e == Tint = Tint
    | otherwise = error "Failed to get type @ succ"
-- T-Pred
getType env (Pred e)
    | getType env e == Tint = Tint
    | otherwise = error "Failed to get type @ pred"
-- T-Iszero
getType env (Iszero e)
    | getType env e == Tint = Tbool
    | otherwise = error "Failed to get type @ iszero"
-- T-Var
getType env (VarUsage (Var s)) = case Map.lookup s env of
    (Just (Left t)) -> t
    _ -> error "Failed to get type @ VarUsage"
-- T-App
getType env (App e1 e2) = handle $ getType env e1
    where
        handle :: Type -> Type
        handle (Arrow t1 t2)
            | t1 == getType env e2 = t2
        handle _ = error "Failed to get type @ App"
-- T-Abs
getType env (Abs (Var var) t e)
    | getKind env t == Star = Arrow t $ getType (Map.insert var (Left t) env) e
    | otherwise = error "Failed to get type @ Abs"
-- T-TAbs
getType env (TypeAbs (Var var) k e) = Forall (Var var) k $ getType (Map.insert var (Right k) env) e
-- T-TApp
getType env (TypeApp e t) = handle $ getType env e
    where
        handle :: Type -> Type
        handle (Forall (Var var) k t2)
            | getKind env t == k = trace "Substitution to be done here [var->t]" t2 -- TODO
        handle _ = error "Failed to get type @ TypeApp"


getKind :: Env -> Type -> Kind
getKind _ Tint = Star
getKind _ Tbool = Star
-- K-Arrow
getKind env (Arrow t₁ t₂)
    | getKind env t₁ == Star && getKind env t₂ == Star = Star
getKind env (TypeVarUsage (Var s)) = case Map.lookup s env of
    (Just (Right k)) -> k
    _ -> error "Failed to get kind @ TypeVarUsage"
getKind e t = trace (show t) Star
-- TODO
