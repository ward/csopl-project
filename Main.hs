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

-- |Recursively applies typing rules to find the type of an expression.
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
-- T-TApp: t1 T2
getType env (TypeApp t₁ t₂) = handle $ getType env t₁
    where
        handle :: Type -> Type
        handle (Forall (Var x) k₁₁ t₁₂)
            | getKind env t₂ == k₁₁ = substituteType x t₂ t₁₂
        handle _ = error "Failed to get type @ TypeApp"
-- T-Eq? TODO

-- |Recursively applies kinding rules to find the kind of a given type
getKind :: Env -> Type -> Kind
getKind _ Tint = Star
getKind _ Tbool = Star
-- K-TVar
getKind env (TypeVarUsage (Var x)) = case Map.lookup x env of
    (Just (Right k)) -> k
    _ -> error "Failed to get kind @ TypeVarUsage"
-- K-Abs
getKind env (OpAbs (Var x) k₁ t₂) = KindArrow k₁ $ getKind (Map.insert x (Right k₁) env) t₂
-- K-App
getKind env (OpApp t₁ t₂) = handle $ getKind env t₁
    where
        handle :: Kind -> Kind
        handle (KindArrow k₁₁ k₁₂)
            | k₁₁ == getKind env t₂ = k₁₂
        handle _ = error "Failed to get kind @ OpApp"
-- K-Arrow
getKind env (Arrow t₁ t₂)
    | getKind env t₁ == Star && getKind env t₂ == Star = Star
    | otherwise = "Failed to get kind @ Arrow"
-- K-All
getKind env (Forall (Var x) k₁ t₂)
    | getKind (Map.insert x (Right k₁) env) t₂ == Star = Star

-- |Used to perform substitution in T-TApp.
substituteType :: String -> Type -> Type -> Type
substituteType s arg Tint = Tint
substituteType s arg Tbool = Tbool
substituteType s arg tvu@(TypeVarUsage (Var var))
    | s == var = arg
    | otherwise = tvu
substituteType s arg (Arrow t1 t2)
    = Arrow (substituteType s arg t1) (substituteType s arg t2)
substituteType s arg fa@(Forall (Var var) kind t)
    | s == var = trace "TODO should it rename quantifier variable of forall or skip?" fa -- TODO
    | otherwise = Forall (Var var) kind (substituteType s arg t)
substituteType s arg opabs@(OpAbs (Var var) kind t)
    | s == var = opabs
    | otherwise = OpAbs (Var var) kind $ substituteType s arg t
substituteType s arg (OpApp t1 t2)
    = OpApp (substituteType s arg t1) (substituteType s arg t2)