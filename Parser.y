{
module Parser (
    calc,
    lexer,
    Exp(..),
    Bbool(..),
    LambdaVar(..),
    Type(..),
    Variable(..)) where

-- EDIT THE .y FILE, NOT THE .hs file
-- The .hs file is generated using happy

import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }

%right '→'

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
    '(' { TokenOpenBracket }
    ')' { TokenCloseBracket }
    '[' { TokenOpenSqBracket }
    ']' { TokenCloseSqBracket }
    add { TokenAdd }
    mult { TokenMult }
    sub { TokenSub }
    div { TokenDiv }
    while { TokenWhile }
    app { TokenApp }
    'λ' { TokenLambda }
    ':' { TokenColon }
    '.' { TokenDot }
    '→' { TokenArrow }
    var { TokenVar $$ }
    vartype { TokenType $$ }

%%

Exp
    : '(' Exp ')' { $2 }
    | if Exp then Exp else Exp { If $2 $4 $6 }
    | Bbool { Bbool $1 }
    | succ Exp { Succ $2 }
    | pred Exp { Pred $2 }
    | '0' { Zero }
    | iszero Exp { Iszero $2 }
    | add Exp Exp { Add $2 $3 }
    | mult Exp Exp { Mult $2 $3 }
    | sub Exp Exp { Sub $2 $3 }
    | div Exp Exp { Div $2 $3 }
    | while Exp Exp { While $2 $3 }
    | app Exp Exp { App $2 $3 }
    | '[' 'λ' LambdaVar '.' Exp ']' { Lambda $3 $5 }
    | Variable { VarUsage $1 }

Bbool
    : false { Bfalse }
    | true { Btrue }

LambdaVar
    : Variable ':' Type { LambdaVar $1 $3 }

Type
    : vartype { Type $1 }
    | Type '→' Type { Arrow $1 $3 }
    | '(' Type ')' { $2 }

Variable
    : var { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "I'm afraid I cannot parse that"

data Exp
    = Bbool Bbool
    | Zero
    | If Exp Exp Exp
    | Iszero Exp
    | Succ Exp
    | Pred Exp
    | Add Exp Exp
    | Mult Exp Exp
    | Sub Exp Exp
    | Div Exp Exp
    | While Exp Exp
    | Lambda LambdaVar Exp
    | VarUsage Variable
    | App Exp Exp
        deriving (Show, Eq)

data Bbool
    = Bfalse
    | Btrue
        deriving (Show, Eq)

data LambdaVar
    = LambdaVar Variable Type
        deriving (Show, Eq)

data Type
    = Type String
    | Arrow Type Type
        deriving (Show, Eq)

data Variable
    = Var String
        deriving (Show, Eq)

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
    | TokenOpenBracket
    | TokenCloseBracket
    | TokenOpenSqBracket
    | TokenCloseSqBracket
    | TokenAdd
    | TokenMult
    | TokenSub
    | TokenDiv
    | TokenWhile
    | TokenApp
    | TokenLambda
    | TokenColon
    | TokenDot
    | TokenArrow
    | TokenVar String
    | TokenType String
        deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
lexer ('0':cs) = TokenZero : lexer cs
lexer ('(':cs) = TokenOpenBracket : lexer cs
lexer (')':cs) = TokenCloseBracket : lexer cs
lexer ('[':cs) = TokenOpenSqBracket : lexer cs
lexer (']':cs) = TokenCloseSqBracket : lexer cs
lexer ('λ':cs) = TokenLambda : lexer cs
-- To distinguish between free text representing a variable and that
-- representing types, this function handles everything for the type.
-- Upon encountering a '.', control is back to the lexer.
lexer (':':cs) = TokenColon : lexerType cs
  where
    lexerType :: String -> [Token]
    lexerType ('(':cs) = TokenOpenBracket : lexerType cs
    lexerType (')':cs) = TokenCloseBracket : lexerType cs
    lexerType ('→':cs) = TokenArrow : lexerType cs
    lexerType ('.':cs) = lexer $ '.':cs
    lexerType cs =
      case span isAlpha cs of
          (var, rest) -> TokenType var : lexerType rest
lexer ('.':cs) = TokenDot : lexer cs
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
        ("add", rest) -> TokenAdd : lexer rest
        ("mult", rest) -> TokenMult : lexer rest
        ("sub", rest) -> TokenSub : lexer rest
        ("div", rest) -> TokenDiv : lexer rest
        ("while", rest) -> TokenWhile : lexer rest
        ("app", rest) -> TokenApp : lexer rest
        (var, rest) -> TokenVar var : lexer rest

}
