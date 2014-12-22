module Main where

import qualified Parser as P
import qualified Data.Map as Map

data Type
    = Bool
    | Nat
    | Arrow Type Type
    | BadlyTyped
        deriving (Eq)
instance Show Type where
    show Bool = "Bool"
    show Nat = "Nat"
    show (Arrow t1 t2) = "(→ " ++ show t1 ++ " " ++ show t2 ++ ")"
    show BadlyTyped = "BadlyTyped"

data Kind
    = Star
    | KindArrow Kind Kind
        deriving (Eq)
instance Show Kind where
    show Star = "*"
    show (KindArrow a b) = "(⇒ " ++ show a ++ " " ++ show b ++ ")"

-- type synonym
type Env = Map.Map String Type

data Value = Vtrue | Vfalse | NumVal NumValue | VLambda P.Variable P.Type P.Exp
    deriving (Show, Eq)
data NumValue = Zero | Succ NumValue
    deriving (Show, Eq)

main = do
    content <- getContents
    let parsed = P.calc . P.lexer $ content
    putStr "Input:     "
    putStr content
    putStr "Parsed:    "
    print parsed
    putStr "Type:      "
    print . findType $ parsed
    putStr "Evaluated: "
    print . eval $ parsed

findType :: P.Exp -> Type
findType exp = getType Map.empty exp

getType :: Env -> P.Exp -> Type
-- T-True
getType _ P.Btrue = Bool
-- T-False
getType _ P.Bfalse = Bool
-- T-If
getType env (P.If c t f)
    | getType env c == Bool && getType env t == getType env f = getType env t
    | otherwise = BadlyTyped
-- T-Zero
getType _ P.Zero = Nat
-- T-Succ
getType env (P.Succ e)
    | getType env e == Nat = Nat
    | otherwise = BadlyTyped
-- T-Pred
getType env (P.Pred e)
    | getType env e == Nat = Nat
    | otherwise = BadlyTyped
-- T-Iszero
getType env (P.Iszero e)
    | getType env e == Nat = Bool
    | otherwise = BadlyTyped
getType env (P.Add e1 e2) = getTypeArithmetic env e1 e2
getType env (P.Mult e1 e2) = getTypeArithmetic env e1 e2
getType env (P.Sub e1 e2) = getTypeArithmetic env e1 e2
getType env (P.Div e1 e2) = getTypeArithmetic env e1 e2
--getType env (P.While c body)
--    | getType env c == Bool = getType env body
--    | otherwise = BadlyTyped
-- T-Var
getType env (P.VarUsage (P.Var s))
    | Map.member s env = env Map.! s
    | otherwise = BadlyTyped
-- T-App
getType env (P.App e1 e2)
    | getFirst (getType env e1) == getType env e2
        && getType env e2 /= BadlyTyped
        = getSecond $ getType env e1
    | otherwise = BadlyTyped
        where
            getFirst :: Type -> Type
            getFirst (Arrow t1 t2) = t1
            getFirst _ = BadlyTyped
            getSecond :: Type -> Type
            getSecond (Arrow t1 t2) = t2
            getSecond _ = BadlyTyped
-- T-Abs
getType env (P.Lambda (P.Var var) t exp)
    | vartype /= BadlyTyped && bodytype /= BadlyTyped = Arrow vartype bodytype
    | otherwise = BadlyTyped
        where
            bodytype :: Type
            bodytype = getType (Map.insert var vartype env) exp
            vartype :: Type
            vartype = readTypeDeclaration t

-- The type declaration for the parameter of a lambda expression requires some
-- special handling.
readTypeDeclaration :: P.Type -> Type
readTypeDeclaration P.Tint = Nat
readTypeDeclaration P.Tbool = Bool
readTypeDeclaration (P.Arrow t1 t2)
    | readTypeDeclaration t1 /= BadlyTyped
        && readTypeDeclaration t2 /= BadlyTyped
        = Arrow (readTypeDeclaration t1) (readTypeDeclaration t2)
readTypeDeclaration _ = BadlyTyped

getTypeArithmetic :: Env -> P.Exp -> P.Exp -> Type
getTypeArithmetic env e1 e2
    | getType env e1 == Nat && getType env e2 == Nat = Nat
    | otherwise = BadlyTyped


eval :: P.Exp -> Value
eval P.Btrue = Vtrue
eval P.Bfalse = Vfalse
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
--eval (P.While e1 e2) = while $ eval e1
--    where
--        while Vtrue  = eval $ P.While (P.Bbool P.Btrue) e2
--        while Vfalse = error "Nothing left when trying to eval while"
--        while _      = error "Evaluation failed @ while"
eval (P.Lambda var t body) = VLambda var t body
eval (P.App e1 e2) = app (eval e1) (eval e2)
    where
        app :: Value -> Value -> Value
        app (VLambda (P.Var varname) t body) arg = eval $ substitute varname (val2exp arg) body
        app _ _ = error "Evaluation failed @ app"

-- Do we want to move this to the parser file?
substitute :: String -> P.Exp -> P.Exp -> P.Exp
substitute s arg (P.VarUsage (P.Var s2))
    | s == s2 = arg
    | otherwise = P.VarUsage $ P.Var s2
substitute s arg l@(P.Lambda v@(P.Var s2) t b)
    | s == s2 = l
    | otherwise = P.Lambda v t $ substitute s arg b
substitute s arg P.Zero = P.Zero
substitute s arg P.Btrue = P.Btrue
substitute s arg P.Bfalse = P.Bfalse
substitute s arg (P.If c t f) = P.If (substitute s arg c) (substitute s arg t) (substitute s arg f)
substitute s arg (P.Iszero e) = P.Iszero $ substitute s arg e
substitute s arg (P.Succ e) = P.Succ $ substitute s arg e
substitute s arg (P.Pred e) = P.Pred $ substitute s arg e
substitute s arg (P.Add e1 e2) = P.Add (substitute s arg e1) (substitute s arg e2)
substitute s arg (P.Mult e1 e2) = P.Mult (substitute s arg e1) (substitute s arg e2)
substitute s arg (P.Sub e1 e2) = P.Sub (substitute s arg e1) (substitute s arg e2)
substitute s arg (P.Div e1 e2) = P.Div (substitute s arg e1) (substitute s arg e2)
--substitute s arg (P.While e1 e2) = P.While (substitute s arg e1) (substitute s arg e2)
substitute s arg (P.App e1 e2) = P.App (substitute s arg e1) (substitute s arg e2)

-- Grmbl, we need to be able to throw an Exp back at eval
val2exp :: Value -> P.Exp
val2exp Vtrue = P.Btrue
val2exp Vfalse = P.Bfalse
val2exp (VLambda v t b) = P.Lambda v t b
val2exp (NumVal nv) = num2exp nv
num2exp :: NumValue -> P.Exp
num2exp Zero = P.Zero
num2exp (Succ n) = P.Succ $ num2exp n
