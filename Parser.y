{
module Parser (calc, lexer, Exp(..), Bbool(..)) where

-- EDIT THE .y FILE, NOT THE .hs file
-- The .hs file is generated using happy

import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    true { TokenTrue }
    false { TokenFalse }
    if { TokenIf }
    then { TokenThen }
    else { TokenElse }
    '0' { TokenZero }
    succ { TokenSucc }
    pred { TokenPred }
    iszero { TokenIszero }

%%

Exp
    : if Exp then Exp else Exp { If $2 $4 $6 }
    | Bbool { Bbool $1 }
    | succ Exp { Succ $2 }
    | pred Exp { Pred $2 }
    | '0' { Zero }
    | iszero Exp { Iszero $2 }

Bbool
    : false { Bfalse }
    | true { Btrue }

{
parseError :: [Token] -> a
parseError _ = error "Error parsing your shit"

data Exp
    = Bbool Bbool
    | Zero
    | If Exp Exp Exp
    | Iszero Exp
    | Succ Exp
    | Pred Exp
        deriving (Show)

data Bbool
    = Bfalse
    | Btrue
        deriving (Show)

data Token
    = TokenTrue
    | TokenFalse
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenZero
    | TokenSucc
    | TokenPred
    | TokenIszero
        deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
lexer ('0':cs) = TokenZero : lexer cs
lexer cs =
    case span isAlpha cs of
        ("if", rest) -> TokenIf : lexer rest
        ("then", rest) -> TokenThen : lexer rest
        ("else", rest) -> TokenElse : lexer rest
        ("iszero", rest) -> TokenIszero : lexer rest
        ("succ", rest) -> TokenSucc : lexer rest
        ("pred", rest) -> TokenPred : lexer rest
        ("true", rest) -> TokenTrue : lexer rest
        ("false", rest) -> TokenFalse : lexer rest

}