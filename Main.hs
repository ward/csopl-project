module Main where

import Parser
import qualified Data.Map as Map
import Debug.Trace

type Env = Map.Map String (Either Type Kind)

main = do
    content <- getContents
    let parsed = calc . lexer $ content
    putStrLn "+----------------------------------------------------------+"
    putStrLn "|         Input                                            |"
    putStrLn "+----------------------------------------------------------+"
    putStr content
    putStrLn "+----------------------------------------------------------+"
    putStrLn "|         Parsed                                           |"
    putStrLn "+----------------------------------------------------------+"
    mapM_ print parsed
    putStrLn "+----------------------------------------------------------+"
    putStrLn "|         Type                                             |"
    putStrLn "+----------------------------------------------------------+"
    print . findType $ parsed
    putStrLn "+----------------------------------------------------------+"
    putStrLn "|         Evaluated                                        |"
    putStrLn "+----------------------------------------------------------+"
    print . evalAll $ parsed


--  _______          _
-- |__   __|        (_)
--    | |_   _ _ __  _ _ __   __ _
--    | | | | | '_ \| | '_ \ / _` |
--    | | |_| | |_) | | | | | (_| |
--    |_|\__, | .__/|_|_| |_|\__, |
--        __/ | |             __/ |
--       |___/|_|            |___/
findType :: [Exp] -> Type
findType exps = getTypeAll Map.empty exps

-- |This is a pure language where only the last expression "returns" to the real
--  world. As such, ignore those that have no relevance whatsoever.
getTypeAll :: Env -> [Exp] -> Type
getTypeAll env ((Define (Var x) exp):[]) = getType env exp
getTypeAll env ((Define (Var x) exp):es) = getTypeAll (Map.insert x (Left (getType env exp)) env) es
getTypeAll env (e:[]) = getType env e
getTypeAll env (e:es) = getTypeAll env es

-- Hackish way to force evaluation of getType
--getTypeAll env (e:es)
--    | okType (getType env e) = getTypeAll env es
--    where
--        okType :: Type -> Bool
--        okType Tbool = True
--        okType Tint = True
--        okType (TypeVarUsage _) = True
--        okType (Arrow _ _) = True
--        okType (Forall _ _ _) = True
--        okType (OpAbs _ _ _) = True
--        okType (OpApp _ _) = True

-- |Recursively applies typing rules to find the type of an expression.
getType :: Env -> Exp -> Type
-- T-True
getType _ Btrue = Tbool
-- T-False
getType _ Bfalse = Tbool
-- T-If
getType env (If c t f)
    | eqType (getType env c) Tbool
        && eqType (getType env t) (getType env f) = getType env t
    | otherwise = error "Failed to get type @ if"
-- T-Zero
getType _ Zero = Tint
-- T-Succ
getType env (Succ e)
    | eqType (getType env e) Tint = Tint
    | otherwise = error "Failed to get type @ succ"
-- T-Pred
getType env (Pred e)
    | eqType (getType env e) Tint = Tint
    | otherwise = error "Failed to get type @ pred"
-- T-Iszero
getType env (Iszero e)
    | eqType (getType env e) Tint = Tbool
    | otherwise = error "Failed to get type @ iszero"
-- T-Var
getType env (VarUsage (Var s)) = case Map.lookup s env of
    (Just (Left t)) -> t
    _ -> error $ "Failed to get type @ VarUsage of " ++ show s
-- T-App
getType env (App e1 e2) = handle $ getType env e1
    where
        handle :: Type -> Type
        handle (Arrow t1 t2)
            | eqType t1 (getType env e2) = t2
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
            | getKind env t₂ == k₁₁ = substituteTypeInType x t₂ t₁₂
        handle _ = error "Failed to get type @ TypeApp"
-- T-Define
getType env (Define v e) = getType env e

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
    | otherwise = error "Failed to get kind @ Arrow"
-- K-All
getKind env (Forall (Var x) k₁ t₂)
    | getKind (Map.insert x (Right k₁) env) t₂ == Star = Star

-- |Check whether two given types can be considered equivalent.
--  Should probably be used everywhere we would have used t₁ == t₂.
--  Based on T-Eq and all the Q-*
eqType :: Type -> Type -> Bool
-- If Haskell can do it, then do not worry.
eqType t₁ t₂
    | t₁ == t₂ = True
-- We can assume this since we check for same kinds before reaching this
eqType (TypeVarUsage x) (TypeVarUsage y) = True
-- TODO How the hell do you do Q-Trans? ...
-- Q-Arrow
eqType (Arrow s₁ s₂) (Arrow t₁ t₂) = eqType s₁ t₁ && eqType s₂ t₂
-- Q-All
eqType (Forall x k₁ t₁) (Forall y k₂ t₂) = k₁ == k₂ && eqType t₁ t₂
-- Q-Abs
eqType (OpAbs x k₁ t₁) (OpAbs y k₂ t₂) = k₁ == k₂ && eqType t₁ t₂
-- Q-App
eqType (OpApp s₁ s₂) (OpApp t₁ t₂) = eqType s₁ t₁ && eqType s₂ t₂
-- Q-AppAbs
eqType (OpApp (OpAbs (Var x) k t₁₂) t₂) t
    = eqType t $ substituteTypeInType x t₂ t₁₂
eqType t (OpApp (OpAbs (Var x) k t₁₂) t₂)
    = eqType t $ substituteTypeInType x t₂ t₁₂
eqType t₁ t₂ = False

--  ______          _             _   _
-- |  ____|        | |           | | (_)
-- | |____   ____ _| |_   _  __ _| |_ _  ___  _ __
-- |  __\ \ / / _` | | | | |/ _` | __| |/ _ \| '_ \
-- | |___\ V / (_| | | |_| | (_| | |_| | (_) | | | |
-- |______\_/ \__,_|_|\__,_|\__,_|\__|_|\___/|_| |_|

-- |Evaluates several expressions. Since there are no sideeffects, we really
--  only consider the last expression and any defines before it.
evalAll :: [Exp] -> Exp
evalAll ((Define (Var x) exp):[]) = eval exp
evalAll ((Define (Var x) exp):es) = evalAll $ substituteTermInTerms x (eval exp) es
    where
        substituteTermInTerms :: String -> Exp -> [Exp] -> [Exp]
        substituteTermInTerms _ _ [] = []
        substituteTermInTerms s arg ((Define (Var x) e):es)
            | s == x = Define (Var x) (substituteTermInTerm s arg e) : es
        substituteTermInTerms s arg (e:es)
            = substituteTermInTerm s arg e : substituteTermInTerms s arg es
evalAll (e:[]) = eval e
evalAll (e:es) = evalAll es


-- | Applies the evaluation rules E-*
eval :: Exp -> Exp
eval Btrue = Btrue
eval Bfalse = Bfalse
eval Zero = Zero
eval (Abs v t e) = Abs v t e
eval (TypeAbs v k e) = TypeAbs v k e
-- E-If, E-IfTrue, E-IfFalse
eval (If c t f) = chooseBranch $ eval c
    where
        chooseBranch :: Exp -> Exp
        chooseBranch Btrue = eval t
        chooseBranch Bfalse = eval f
        chooseBranch _ = error "Evaluation failed @ if"
-- E-Succ
eval (Succ t) = Succ $ eval t
-- E-PredZero
eval (Pred Zero) = Zero
-- E-PredSucc
eval (Pred (Succ nv))
    | isNumericValue nv = nv
-- E-Pred
eval (Pred t) = eval $ Pred $ eval t
-- E-IsZeroZero
eval (Iszero Zero) = Btrue
-- E-IsZeroSucc
eval (Iszero (Succ nv))
    | isNumericValue nv = Bfalse
-- E-IsZero
eval (Iszero t) = eval $ Iszero $ eval t
-- E-AppAbs
eval (App (Abs (Var x) t₁₁ t₁₂) v₂)
    | isValue v₂ = eval $ substituteTermInTerm x v₂ t₁₂
-- E-App2
eval (App v₁ t₂)
    | isValue v₁ = eval $ App v₁ (eval t₂)
-- E-App1
eval (App t₁ t₂) = eval $ App (eval t₁) t₂
-- E-TappTabs
eval (TypeApp (TypeAbs (Var x) k₁₁ t₁₂) t₂) = eval $ substituteTypeInTerm x t₂ t₁₂
-- E-TApp
eval (TypeApp t₁ t₂) = eval $ TypeApp (eval t₁) t₂
-- E-Define
eval (Define v e) = eval e


-- |Decides whether something is a value (true, false, abs, type abs, numerical
--  value)
isValue :: Exp -> Bool
isValue Btrue = True
isValue Bfalse = True
isValue (Abs v t e) = True
isValue (TypeAbs v k e) = True
isValue nv = isNumericValue nv
-- |Decides whether something is a numerical value (Zero or succ of numerical
--  value)
isNumericValue :: Exp -> Bool
isNumericValue Zero = True
isNumericValue (Succ nv) = isNumericValue nv
isNumericValue _ = False


--   _____       _         _   _ _         _   _
--  / ____|     | |       | | (_) |       | | (_)
-- | (___  _   _| |__  ___| |_ _| |_ _   _| |_ _  ___  _ __
--  \___ \| | | | '_ \/ __| __| | __| | | | __| |/ _ \| '_ \
--  ____) | |_| | |_) \__ \ |_| | |_| |_| | |_| | (_) | | | |
-- |_____/ \__,_|_.__/|___/\__|_|\__|\__,_|\__|_|\___/|_| |_|


-- |Used to perform substitution, placing a type in a type.
substituteTypeInType :: String -> Type -> Type -> Type
substituteTypeInType s arg Tint = Tint
substituteTypeInType s arg Tbool = Tbool
substituteTypeInType s arg tvu@(TypeVarUsage (Var var))
    | s == var = arg
    | otherwise = tvu
substituteTypeInType s arg (Arrow t1 t2)
    = Arrow (substituteTypeInType s arg t1) (substituteTypeInType s arg t2)
substituteTypeInType s arg fa@(Forall (Var var) kind t)
    | s == var = trace "TODO should it rename quantifier variable of forall or skip?" fa -- TODO
    | otherwise = Forall (Var var) kind (substituteTypeInType s arg t)
substituteTypeInType s arg opabs@(OpAbs (Var var) kind t)
    | s == var = opabs
    | otherwise = OpAbs (Var var) kind $ substituteTypeInType s arg t
substituteTypeInType s arg (OpApp t1 t2)
    = OpApp (substituteTypeInType s arg t1) (substituteTypeInType s arg t2)

-- |Used to perform substitution, placing a term in a term.
substituteTermInTerm :: String -> Exp -> Exp -> Exp
substituteTermInTerm s arg Btrue = Btrue
substituteTermInTerm s arg Bfalse = Bfalse
substituteTermInTerm s arg Zero = Zero
substituteTermInTerm s arg (VarUsage (Var s2))
    | s == s2 = arg
    | otherwise = VarUsage $ Var s2
substituteTermInTerm s arg (If c t f)
    = If (substituteTermInTerm s arg c)
         (substituteTermInTerm s arg t)
         (substituteTermInTerm s arg f)
substituteTermInTerm s arg (Succ e) = Succ $ substituteTermInTerm s arg e
substituteTermInTerm s arg (Pred e) = Pred $ substituteTermInTerm s arg e
substituteTermInTerm s arg (Iszero e) = Iszero $ substituteTermInTerm s arg e
substituteTermInTerm s arg abstraction@(Abs (Var x) t b)
    | s == x = abstraction
    | otherwise = Abs (Var x) t $ substituteTermInTerm s arg b
substituteTermInTerm s arg (App e1 e2) = App (substituteTermInTerm s arg e1) (substituteTermInTerm s arg e2)
substituteTermInTerm s arg tabs@(TypeAbs (Var x) k e)
    | s == x = tabs
    | otherwise = TypeAbs (Var x) k $ substituteTermInTerm s arg e
substituteTermInTerm s arg (TypeApp e t) = TypeApp (substituteTermInTerm s arg e) t
substituteTermInTerm s arg (Define v e) = Define v $ substituteTermInTerm s arg e

-- |Used to perform substitution, placing a type in a term.
substituteTypeInTerm :: String -> Type -> Exp -> Exp
substituteTypeInTerm s arg Btrue = Btrue
substituteTypeInTerm s arg Bfalse = Bfalse
substituteTypeInTerm s arg Zero = Zero
substituteTypeInTerm s arg (VarUsage (Var s2))
    | s == s2 = error "Encountered variable representing a type where I shouldn't"
    | otherwise = VarUsage $ Var s2
substituteTypeInTerm s arg (If c t f)
    = If (substituteTypeInTerm s arg c)
         (substituteTypeInTerm s arg t)
         (substituteTypeInTerm s arg f)
substituteTypeInTerm s arg (Succ e) = Succ $ substituteTypeInTerm s arg e
substituteTypeInTerm s arg (Pred e) = Pred $ substituteTypeInTerm s arg e
substituteTypeInTerm s arg (Iszero e) = Iszero $ substituteTypeInTerm s arg e
substituteTypeInTerm s arg abstraction@(Abs (Var x) t b)
    | s == x = abstraction
    | otherwise = Abs (Var x) (substituteTypeInType s arg t) (substituteTypeInTerm s arg b)
substituteTypeInTerm s arg (App e1 e2)
    = App (substituteTypeInTerm s arg e1) (substituteTypeInTerm s arg e2)
substituteTypeInTerm s arg tabs@(TypeAbs (Var x) k e)
    | s == x = tabs
    | otherwise = TypeAbs (Var x) k $ substituteTypeInTerm s arg e
substituteTypeInTerm s arg (TypeApp e t)
    = TypeApp (substituteTypeInTerm s arg e) (substituteTypeInType s arg t)
substituteTypeInTerm s arg (Define v e) = Define v $ substituteTypeInTerm s arg e
