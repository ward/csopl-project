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
    print . eval $ parsed

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


eval :: P.Exp -> Value
eval (P.Bbool P.Btrue) = Vtrue
eval (P.Bbool P.Bfalse) = Vfalse
eval P.Zero = NumVal Zero
eval (P.Iszero e) = iszero $ eval e
    where
        iszero (NumVal Zero)     = Vtrue
        iszero (NumVal (Succ _)) = Vfalse
        iszero _                 = error "Evaluation failed @ iszero"
-- Slightly stricter than the actual evaluation rules
eval (P.Succ e) = succ $ eval e
    where
        succ (NumVal nv) = NumVal $ Succ nv
        succ _           = error "Evaluation failed @ succ"
eval (P.Pred e) = pred $ eval e
    where
        pred (NumVal Zero)      = NumVal Zero
        pred (NumVal (Succ nv)) = NumVal nv
        pred _                  = error "Evaluation failed @ pred"
eval (P.If c t f) = eif $ eval c
    where
        eif Vtrue  = eval t
        eif Vfalse = eval f
        eif _      = error "Evaluation failed @ if"
eval (P.Add e1 e2) = add (eval e1) (eval e2)
    where
        add nv@(NumVal _) (NumVal Zero)      = nv
        add (NumVal nv1) (NumVal (Succ nv2)) = addRec $ eval (P.Add (num2exp nv1) (num2exp nv2))
        add _ _                              = error "Evaluation failed @ add"
        addRec (NumVal nv) = NumVal $ Succ nv
        addRec _           = error "Evaluation failed @ add"
eval (P.Mult e1 e2) = mult (eval e1) (eval e2)
    where
        mult (NumVal _) (NumVal Zero)         = NumVal Zero
        mult (NumVal nv1) (NumVal (Succ nv2)) = multRec nv1 (eval (P.Mult (num2exp nv1) (num2exp nv2)))
        mult _ _                              = error "Evaluation failed @ mult"
        multRec nv1 (NumVal nv2) = eval $ P.Add (num2exp nv1) (num2exp nv2)
        multRec _ _              = error "Evaluation failed @ mult"
eval (P.Sub e1 e2) = sub (eval e1) (eval e2)
    where
        sub nv@(NumVal _) (NumVal Zero)             = nv
        sub (NumVal Zero) (NumVal (Succ _))         = NumVal Zero
        sub (NumVal (Succ nv1)) (NumVal (Succ nv2)) = eval $ P.Sub (num2exp nv1) (num2exp nv2)
        sub _ _                                     = error "Evaluation failed @ sub"
-- Only gives exactly what you expect *if* there is no remainder
eval (P.Div e1 e2) = divv (eval e1) (eval e2)
    where
        divv (NumVal Zero) (NumVal (Succ nv))        = NumVal Zero
        divv (NumVal (Succ nv1)) (NumVal (Succ nv2)) =
            divvRec $ eval (P.Div (P.Sub (num2exp nv1) (num2exp nv2)) (P.Succ (num2exp nv2)))
        divv _ _                                     = error "Evaluation failed @ div"
        divvRec (NumVal nv) = NumVal (Succ nv)
        divvRec _           = error "Evaluation failed @ div"
eval (P.While e1 e2) = while $ eval e1
    where
        while Vtrue  = eval $ P.While (P.Bbool P.Btrue) e2
        while Vfalse = error "Nothing left when trying to eval while"
        while _      = error "Evaluation failed @ while"

-- Grmbl, we need to be able to throw an Exp back at eval
val2exp :: Value -> P.Exp
val2exp Vtrue = P.Bbool P.Btrue
val2exp Vfalse = P.Bbool P.Bfalse
val2exp (NumVal nv) = num2exp nv
num2exp :: NumValue -> P.Exp
num2exp Zero = P.Zero
num2exp (Succ n) = P.Succ $ num2exp n
