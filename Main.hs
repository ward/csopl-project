module Main where

import qualified Parser as P
import qualified Data.Map as Map
import Debug.Trace

data Type
    = Bool
    | Nat
    | Arrow Type Type
    | Forall P.Variable Kind Type
    | BadlyTyped
        deriving (Eq)
instance Show Type where
    show Bool = "Bool"
    show Nat = "Nat"
    show (Arrow t1 t2) = "(→ " ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Forall v k t) = "(∀ " ++ show v ++ " " ++ show k ++ " " ++ show t ++ ")"
    show BadlyTyped = "BadlyTyped"

data Kind
    = Star
    | KindArrow Kind Kind
        deriving (Eq)
instance Show Kind where
    show Star = "*"
    show (KindArrow a b) = "(⇒ " ++ show a ++ " " ++ show b ++ ")"

-- type synonym
type TypeEnv = Map.Map String Type
type KindEnv = Map.Map String Kind

data Value
    = Vtrue
    | Vfalse
    | NumVal NumValue
    | VAbs P.Variable P.Type P.Exp
    | VTypeAbs P.Variable P.Kind P.Exp
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
findType exp = getType Map.empty Map.empty exp

getType :: TypeEnv -> KindEnv -> P.Exp -> Type
-- T-True
getType _ _ P.Btrue = Bool
-- T-False
getType _ _ P.Bfalse = Bool
-- T-If
getType tenv kenv (P.If c t f)
    | getType tenv kenv c == Bool && getType tenv kenv t == getType tenv kenv f = getType tenv kenv t
    | otherwise = BadlyTyped
-- T-Zero
getType _ _ P.Zero = Nat
-- T-Succ
getType tenv kenv (P.Succ e)
    | getType tenv kenv e == Nat = Nat
    | otherwise = BadlyTyped
-- T-Pred
getType tenv kenv (P.Pred e)
    | getType tenv kenv e == Nat = Nat
    | otherwise = BadlyTyped
-- T-Iszero
getType tenv kenv (P.Iszero e)
    | getType tenv kenv e == Nat = Bool
    | otherwise = BadlyTyped
getType tenv kenv (P.Add e1 e2) = getTypeArithmetic tenv kenv e1 e2
getType tenv kenv (P.Mult e1 e2) = getTypeArithmetic tenv kenv e1 e2
getType tenv kenv (P.Sub e1 e2) = getTypeArithmetic tenv kenv e1 e2
getType tenv kenv (P.Div e1 e2) = getTypeArithmetic tenv kenv e1 e2
--getType tenv kenv (P.While c body)
--    | getType tenv kenv c == Bool = getType tenv kenv body
--    | otherwise = BadlyTyped
-- T-Var
getType tenv kenv (P.VarUsage (P.Var s))
    | Map.member s tenv = tenv Map.! s
    | otherwise = BadlyTyped
-- T-App
getType tenv kenv (P.App e1 e2)
    | getFirst (getType tenv kenv e1) == getType tenv kenv e2
        && getType tenv kenv e2 /= BadlyTyped
        = getSecond $ getType tenv kenv e1
    | otherwise = BadlyTyped
        where
            getFirst :: Type -> Type
            getFirst (Arrow t1 t2) = t1
            getFirst _ = BadlyTyped
            getSecond :: Type -> Type
            getSecond (Arrow t1 t2) = t2
            getSecond _ = BadlyTyped
-- T-Abs
getType tenv kenv (P.Abs (P.Var var) t e)
    | vartype /= BadlyTyped
        && getKind tenv kenv vartype == Star
        && bodytype /= BadlyTyped = Arrow vartype bodytype
    | otherwise = BadlyTyped
        where
            bodytype :: Type
            bodytype = getType (Map.insert var vartype tenv) kenv e
            vartype :: Type
            vartype = readTypeDeclaration t
-- T-TAbs
getType tenv kenv (P.TypeAbs (P.Var var) k e)
    | bodytype /= BadlyTyped = Forall (P.Var var) varkind bodytype
    | otherwise = BadlyTyped
        where
            varkind :: Kind
            varkind = readKindDeclaration k
            bodytype :: Type
            bodytype = getType tenv (Map.insert var varkind kenv) e

readKindDeclaration :: P.Kind -> Kind
readKindDeclaration P.Star = Star
readKindDeclaration (P.KindArrow k₁ k₂) =
    KindArrow (readKindDeclaration k₁) (readKindDeclaration k₂)

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

getTypeArithmetic :: TypeEnv -> KindEnv -> P.Exp -> P.Exp -> Type
getTypeArithmetic tenv kenv e1 e2
    | getType tenv kenv e1 == Nat && getType tenv kenv e2 == Nat = Nat
    | otherwise = BadlyTyped

getKind :: TypeEnv -> KindEnv -> Type -> Kind
getKind _ _ Nat = Star
getKind _ _ Bool = Star
-- K-Arrow
getKind tenv kenv (Arrow t1 t2)
    | getKind tenv kenv t1 == Star && getKind tenv kenv t2 == Star = Star

--------------------------------------------------------------------------------
--------- EVALUATION -----------------------------------------------------------
--------------------------------------------------------------------------------

eval :: P.Exp -> Value
eval P.Btrue = Vtrue
eval P.Bfalse = Vfalse
eval P.Zero = NumVal Zero
-- E-Iszero, E-IszeroZero, E-IszeroSucc
eval (P.Iszero e) = iszero $ eval e
    where
        iszero (NumVal Zero)     = Vtrue
        iszero (NumVal (Succ _)) = Vfalse
        iszero _                 = error "Evaluation failed @ iszero"
-- E-Succ
-- Slightly stricter than the actual evaluation rule (only allow the
-- subterm to be a numerical value)
eval (P.Succ e) = succ $ eval e
    where
        succ (NumVal nv) = NumVal $ Succ nv
        succ _           = error "Evaluation failed @ succ"
-- E-Pred, E-PredZero, E-PredSucc
eval (P.Pred e) = pred $ eval e
    where
        pred (NumVal Zero)      = NumVal Zero
        pred (NumVal (Succ nv)) = NumVal nv
        pred _                  = error "Evaluation failed @ pred"
-- E-If, E-IfTrue, E-IfFalse
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
eval (P.Abs var t body) = VAbs var t body
-- E-App1, E-App2, EAppAbs
eval (P.App e1 e2) = app (eval e1) (eval e2)
    where
        app :: Value -> Value -> Value
        app (VAbs (P.Var varname) t body) arg = eval $ substitute varname (val2exp arg) body
        app _ _ = error "Evaluation failed @ app"
eval (P.TypeAbs var kind body) = VTypeAbs var kind body
-- E-TApp, ETappTabs
eval (P.TypeApp e t) = app (eval e) t
    where
        app :: Value -> P.Type -> Value
        app (VTypeAbs (P.Var varname) kind body) arg = eval $ substituteT varname arg body
        app _ _ = error "Evaluation failed @ typeapp"

--------------------------------------------------------------------------------
--------- SUBSTITUTION ---------------------------------------------------------
-- Do we want to move this to the parser file? ---------------------------------
--------------------------------------------------------------------------------
substitute :: String -> P.Exp -> P.Exp -> P.Exp
substitute s arg (P.VarUsage (P.Var s2))
    | s == s2 = arg
    | otherwise = P.VarUsage $ P.Var s2
substitute s arg l@(P.Abs v@(P.Var s2) t b)
    | s == s2 = l
    | otherwise = P.Abs v t $ substitute s arg b
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

substituteT :: String -> P.Type -> P.Exp -> P.Exp
substituteT s arg (P.VarUsage (P.Var s2))
    | s == s2 = error "Encountered variable representing a type where I shouldn't"
    | otherwise = P.VarUsage $ P.Var s2
substituteT s arg abstraction@(P.Abs v@(P.Var s2) t b)
    | s == s2 = abstraction
    | otherwise = P.Abs v (substituteType s arg t) (substituteT s arg b)
substituteT s arg (P.TypeApp e t) = P.TypeApp (substituteT s arg e) (substituteType s arg t)
substituteT s arg P.Zero = P.Zero
substituteT s arg P.Btrue = P.Btrue
substituteT s arg P.Bfalse = P.Bfalse
substituteT s arg (P.If c t f) = P.If (substituteT s arg c) (substituteT s arg t) (substituteT s arg f)
substituteT s arg (P.Iszero e) = P.Iszero $ substituteT s arg e
substituteT s arg (P.Succ e) = P.Succ $ substituteT s arg e
substituteT s arg (P.Pred e) = P.Pred $ substituteT s arg e
substituteT s arg (P.Add e1 e2) = P.Add (substituteT s arg e1) (substituteT s arg e2)
substituteT s arg (P.Mult e1 e2) = P.Mult (substituteT s arg e1) (substituteT s arg e2)
substituteT s arg (P.Sub e1 e2) = P.Sub (substituteT s arg e1) (substituteT s arg e2)
substituteT s arg (P.Div e1 e2) = P.Div (substituteT s arg e1) (substituteT s arg e2)
--substituteT s arg (P.While e1 e2) = P.While (substitute s arg e1) (substituteT s arg e2)
substituteT s arg (P.App e1 e2) = P.App (substituteT s arg e1) (substituteT s arg e2)

substituteType :: String -> P.Type -> P.Type -> P.Type
substituteType s arg tvu@(P.TypeVarUsage (P.Var var))
    | s == var = arg
    | otherwise = tvu
substituteType s arg opabs@(P.OpAbs (P.Var var) kind t)
    | s == var = opabs
    | otherwise = P.OpAbs (P.Var var) kind $ substituteType s arg t
substituteType s arg fa@(P.Forall (P.Var var) kind t)
    | s == var = trace "TODO should rename variables" fa -- TODO
    | otherwise = P.Forall (P.Var var) kind (substituteType s arg t)
substituteType s arg P.Tint = P.Tint
substituteType s arg P.Tbool = P.Tbool
substituteType s arg (P.Arrow t1 t2)
    = P.Arrow (substituteType s arg t1) (substituteType s arg t2)
substituteType s arg (P.OpApp t1 t2)
    = P.OpApp (substituteType s arg t1) (substituteType s arg t2)

-- Grmbl, we need to be able to throw an Exp back at eval
val2exp :: Value -> P.Exp
val2exp Vtrue = P.Btrue
val2exp Vfalse = P.Bfalse
val2exp (VAbs v t b) = P.Abs v t b
val2exp (NumVal nv) = num2exp nv
num2exp :: NumValue -> P.Exp
num2exp Zero = P.Zero
num2exp (Succ n) = P.Succ $ num2exp n
