module Main where

import qualified Parser as P

data Type = Bool | Nat | BadlyTyped
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
evaluate :: P.Exp -> P.Exp
evaluate (P.Bbool P.Btrue) = P.Bbool P.Btrue
evaluate (P.Bbool P.Bfalse) = P.Bbool P.Bfalse
evaluate (P.Zero) = P.Zero
evaluate (P.Iszero e) = case evaluate e of
    P.Zero -> P.Bbool P.Btrue
    P.Succ _ -> P.Bbool P.Bfalse
    otherwise -> error "Evaluation failed"
evaluate (P.Succ e) = case evaluate e of
    P.Zero -> P.Succ P.Zero
    P.Succ _ -> P.Succ $ evaluate e
    otherwise -> error "Evaluation failed"
evaluate (P.Pred e) = case evaluate e of
    P.Zero -> P.Zero
    P.Succ ev -> ev
    otherwise -> error "Evaluation failed"
evaluate (P.If c t f) = case evaluate c of
    P.Bbool P.Btrue -> evaluate t
    P.Bbool P.Bfalse -> evaluate f
    otherwise -> error "Evaluation failed"
