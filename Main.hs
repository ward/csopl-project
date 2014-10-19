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
