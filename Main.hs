module Main where

import qualified Parser as P
--import Debug.Trace

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
getType (P.Add e1 e2) = getTypeArithmetic e1 e2
getType (P.Mult e1 e2) = getTypeArithmetic e1 e2
getType (P.Sub e1 e2) = getTypeArithmetic e1 e2
getType (P.Div e1 e2) = getTypeArithmetic e1 e2
getType (P.While c body)
    | getType c == Bool = getType body
    | otherwise = BadlyTyped
getTypeArithmetic e1 e2
    | getType e1 == Nat && getType e2 == Nat = Nat
    | otherwise = BadlyTyped


evaluate :: P.Exp -> Value
evaluate (P.Bbool P.Btrue) = Vtrue
evaluate (P.Bbool P.Bfalse) = Vfalse
evaluate (P.Zero) = NumVal Zero
evaluate (P.Iszero e) = case evaluate e of
    NumVal Zero -> Vtrue
    NumVal (Succ _) -> Vfalse
    otherwise -> error "Evaluation failed @ iszero"
evaluate (P.Succ e) = case evaluate e of
    NumVal Zero -> NumVal $ Succ Zero
    NumVal (Succ ev1) -> NumVal $ Succ (Succ ev1)
    otherwise -> error "Evaluation failed @ succ"
evaluate (P.Pred e) = case evaluate e of
    NumVal Zero -> NumVal Zero
    NumVal (Succ ev) -> NumVal ev
    otherwise -> error "Evaluation failed @ pred"
evaluate (P.If c t f) = case evaluate c of
    Vtrue -> evaluate t
    Vfalse -> evaluate f
    otherwise -> error "Evaluation failed @ if"
evaluate (P.Add e1 e2) = case (evaluate e1, evaluate e2) of
    (NumVal _, NumVal Zero) -> evaluate e1
    (NumVal nv1, (NumVal (Succ ev2))) -> case evaluate (P.Add (num2exp nv1) (num2exp ev2)) of
        NumVal nv -> NumVal $ Succ nv
        otherwise -> error "Evaluation failed @ add"
    otherwise -> error "Evaluation failed @ add"
evaluate (P.Mult e1 e2) = case (evaluate e1, evaluate e2) of
    (NumVal _, NumVal Zero) -> NumVal Zero
    (NumVal ev1, NumVal (Succ ev2)) -> case evaluate (P.Mult (num2exp ev1) (num2exp ev2)) of
        NumVal nv -> evaluate $ P.Add (num2exp ev1) (num2exp nv)
        otherwise -> error "Evaluation failed @ mult"
    otherwise -> error "Evaluation failed @ mult"
evaluate (P.Sub e1 e2) = case (evaluate e1, evaluate e2) of
    (NumVal _, NumVal Zero) -> evaluate e1
    (NumVal Zero, NumVal _) -> NumVal Zero
    (NumVal (Succ nv1), NumVal (Succ nv2)) -> evaluate $ P.Sub (num2exp nv1) (num2exp nv2)
    otherwise -> error "Evaluation failed @ sub"
evaluate (P.Div e1 e2) = case (evaluate e1, evaluate e2) of
    (NumVal Zero, NumVal (Succ nv)) -> NumVal Zero
    (NumVal (Succ nv1), NumVal (Succ nv2)) -> case evaluate (P.Div (P.Sub (num2exp nv1) (num2exp nv2)) (P.Succ (num2exp nv2))) of
        NumVal nv -> NumVal (Succ nv)
        otherwise -> error "Evaluation failed @ div"
    otherwise -> error "Evaluation failed @ div"
evaluate (P.While e1 e2) =  case evaluate e1 of
    Vtrue -> evaluate $ P.While (P.Bbool P.Btrue) e2
    Vfalse -> error "Nothing left when trying to evaluate while"
    otherwise -> error "Evaluation failed @ while"

-- Grmbl, we need to be able to throw an Exp back at evaluate
val2exp :: Value -> P.Exp
val2exp Vtrue = P.Bbool P.Btrue
val2exp Vfalse = P.Bbool P.Bfalse
val2exp (NumVal nv) = num2exp nv
num2exp :: NumValue -> P.Exp
num2exp Zero = P.Zero
num2exp (Succ n) = P.Succ $ num2exp n
