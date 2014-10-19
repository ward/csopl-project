module Main where

import qualified Parser as P

data Type = Bool | Nat | BadlyTyped
    deriving (Show, Eq)

data Value = Vtrue | Vfalse | NumVal NumValue
    deriving (Show, Eq)
data NumValue = Zero | Succ NumValue
    deriving (Show, Eq)

main = do
    content <- getContents
    let parsed = P.calc . P.lexer $ content
    putStr "Parsed: "
    print parsed
    putStr "Type: "
    print . getType $ parsed
    putStr "Evaluated: "
    print . evaluate $ parsed

getType :: P.Exp -> Type
getType (P.Bbool P.Btrue) = Bool
getType (P.Bbool P.Bfalse) = Bool
getType (P.If c t f)
    | getType c == Bool && getType t == getType f = getType t
    | otherwise = BadlyTyped
getType P.Zero = Nat
getType (P.Succ e)
    | getType e == Nat = Nat
    | otherwise = BadlyTyped
getType (P.Pred e)
    | getType e == Nat = Nat
    | otherwise = BadlyTyped
getType (P.Iszero e)
    | getType e == Nat = Bool
    | otherwise = BadlyTyped
getType (P.Add e1 e2)
    | getType e1 == Nat && getType e2 == Nat = Nat
    | otherwise = BadlyTyped
getType (P.Mult e1 e2)
    | getType e1 == Nat && getType e2 == Nat = Nat
    | otherwise = BadlyTyped

-- TODO: Rewrite to more strictly follow the rules as described
--       Idea: Rerun eval on it all, only executing a rule at the time
--       If we're done, return what we have
evaluate :: P.Exp -> Value
evaluate (P.Bbool P.Btrue) = Vtrue
evaluate (P.Bbool P.Bfalse) = Vfalse
evaluate (P.Zero) = NumVal Zero
evaluate (P.Iszero e) = case evaluate e of
    NumVal Zero -> Vtrue
    NumVal (Succ _) -> Vfalse
    otherwise -> error "Evaluation failed"
evaluate (P.Succ e) = case evaluate e of
    NumVal Zero -> NumVal $ Succ Zero
    NumVal (Succ ev1) -> NumVal $ Succ (Succ ev1)
    otherwise -> error "Evaluation failed"
evaluate (P.Pred e) = case evaluate e of
    NumVal Zero -> NumVal Zero
    NumVal (Succ ev) -> NumVal ev
    otherwise -> error "Evaluation failed"
evaluate (P.If c t f) = case evaluate c of
    Vtrue -> evaluate t
    Vfalse -> evaluate f
    otherwise -> error "Evaluation failed"
evaluate (P.Add e1 e2) = case (evaluate e1, evaluate e2) of
    (NumVal _, NumVal Zero) -> evaluate e1
    (NumVal nv1, (NumVal (Succ ev2))) -> case evaluate (P.Add (numbertoexp nv1) (numbertoexp ev2)) of
        NumVal nv -> NumVal $ Succ nv
        otherwise -> error "Evaluation failed"
    otherwise -> error "Evaluation failed"
evaluate (P.Mult e1 e2) = case (evaluate e1, evaluate e2) of
    (NumVal _, NumVal Zero) -> NumVal Zero
    (NumVal ev1, NumVal (Succ ev2)) -> case evaluate (P.Mult (numbertoexp ev1) (numbertoexp ev2)) of
        NumVal nv -> evaluate $ P.Add (numbertoexp ev1) (numbertoexp nv)
        otherwise -> error "Evaluation failed"
    otherwise -> error "Evaluation failed"

-- Grmbl, we need to be able to throw an Exp back at evaluate
valuetoexp :: Value -> P.Exp
valuetoexp Vtrue = P.Bbool P.Btrue
valuetoexp Vfalse = P.Bbool P.Bfalse
valuetoexp (NumVal nv) = numbertoexp nv
numbertoexp :: NumValue -> P.Exp
numbertoexp Zero = P.Zero
numbertoexp (Succ n) = P.Succ $ numbertoexp n