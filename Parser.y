{
module Parser (calc, lexer) where

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

%%

Exp
    : if Exp then Exp else Exp { If $2 $4 $6 }
    | Bbool { Bbool $1 }

Bbool
    : false { Bfalse }
    | true { Btrue }

{
parseError :: [Token] -> a
parseError _ = error "Error parsing your shit"

data Exp
    = Bbool Bbool
    | If Exp Exp Exp
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
        deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    | isSpace c = lexer cs
lexer cs =
    case span isAlpha cs of
        ("if", rest) -> TokenIf : lexer rest
        ("then", rest) -> TokenThen : lexer rest
        ("else", rest) -> TokenElse : lexer rest
        ("true", rest) -> TokenTrue : lexer rest
        ("false", rest) -> TokenFalse : lexer rest


--main = getContents >>= print . calc . lexer

}