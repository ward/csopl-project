module Main where

import qualified Parser as P

data Type = Bool | Nat | BadlyTyped
    deriving (Show, Eq)

main = do
    content <- getContents
    putStr "Parsed: "
    print . P.calc . P.lexer $ content
    putStr "Type: "
    print . getType . P.calc . P.lexer $ content

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
