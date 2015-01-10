{
module Parser (
    calc,
    lexer,
    Exp(..),
    Type(..),
    Kind(..),
    Variable(..)) where

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
    'λ' { TokenLambda }
    'Λ' { TokenBigLambda }
    '→' { TokenArrow }
    '∀' { TokenForall }
    var { TokenVar $$ }
    int { TokenTypeInt }
    bool { TokenTypeBool }
    '*' { TokenKind }
    '⇒' { TokenDoubleArrow }
    define { TokenDefine }

%%

Exps
    : Exp { [$1] }
    | Exp Exps { $1 : $2 }

Exp
    : '(' Do ')' { $2 }
    | '[' TApp ']' { $2 }
    | true { Btrue }
    | false { Bfalse }
    | '0' { Zero }
    | Variable { VarUsage $1 }

Do
    : if Exp Exp Exp { If $2 $3 $4 }
    | succ Exp { Succ $2 }
    | pred Exp { Pred $2 }
    | iszero Exp { Iszero $2 }
    | add Exp Exp { Add $2 $3 }
    | mult Exp Exp { Mult $2 $3 }
    | sub Exp Exp { Sub $2 $3 }
    | div Exp Exp { Div $2 $3 }
    | while Exp Exp { While $2 $3 }
    | 'λ' Variable Type Exp { Abs $2 $3 $4 }
    | 'Λ' Variable Kind Exp { TypeAbs $2 $3 $4 }
    | Exp Exp { App $1 $2 }
    | define Variable Exp { Define $2 $3 }

TApp
    : Exp Type { TypeApp $1 $2 }

Type
    : int { Tint }
    | bool { Tbool }
    | Variable { TypeVarUsage $1 }
    | '(' DoType ')' { $2 }

DoType
    : '→' Type Type { Arrow $2 $3 }
    | '∀' Variable Kind Type { Forall $2 $3 $4 }
    | 'λ' Variable Kind Type { OpAbs $2 $3 $4 }
    | Type Type { OpApp $1 $2 }

Kind
    : '*' { Star }
    | '(' '⇒' Kind Kind ')' { KindArrow $3 $4 }

Variable
    : var { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "I'm afraid I cannot parse that"

data Exp
    = Btrue
    | Bfalse
    | Zero
    | VarUsage Variable
    | If Exp Exp Exp
    | Succ Exp
    | Pred Exp
    | Iszero Exp
    | Add Exp Exp
    | Mult Exp Exp
    | Sub Exp Exp
    | Div Exp Exp
    | While Exp Exp
    | Abs Variable Type Exp
    | TypeAbs Variable Kind Exp
    | App Exp Exp
    | TypeApp Exp Type
    | Define Variable Exp
        deriving (Eq)
instance Show Exp where
    show Btrue = "true"
    show Bfalse = "false"
    show Zero = "0"
    show (VarUsage v) = show v
    show (If c t f) = "(if " ++ show c ++ " " ++ show t ++ " " ++ show f ++ ")"
    show (Succ x) = "(succ " ++ show x ++ ")"
    show (Pred x) = "(pred " ++ show x ++ ")"
    show (Iszero x) = "(iszero " ++ show x ++ ")"
    show (Add x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
    show (Mult x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
    show (Sub x y) = "(- " ++ show x ++ " " ++ show y ++ ")"
    show (Div x y) = "(/ " ++ show x ++ " " ++ show y ++ ")"
    show (While c b) = "(while " ++ show c ++ " " ++ show b ++ ")"
    show (Abs v t e) = "(λ " ++ show v ++ " " ++ show t ++ " " ++ show e ++ ")"
    show (TypeAbs v k e) = "(Λ " ++ show v ++ " " ++ show k ++ " " ++ show e ++ ")"
    show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
    show (TypeApp x y) = "[" ++ show x ++ " " ++ show y ++ "]"
    show (Define v e) = "(define " ++ show v ++ " " ++ show e ++ ")"    

data Type
    = Tint
    | Tbool
    | TypeVarUsage Variable
    | Arrow Type Type
    | Forall Variable Kind Type
    | OpAbs Variable Kind Type
    | OpApp Type Type
        deriving (Eq)
instance Show Type where
    show Tint = "int"
    show Tbool = "bool"
    show (TypeVarUsage v) = show v--"(TVU (" ++ show v ++ "))"
    show (Arrow t₁ t₂) = "(→ " ++ show t₁ ++ " " ++ show t₂ ++ ")"
    show (Forall v k t) = "(∀ " ++ show v ++ " " ++ show k ++ " " ++ show t ++ ")"
    show (OpAbs v k t) = "(λ " ++ show v ++ " " ++ show k ++ " " ++ show t ++ ")"
    show (OpApp t₁ t₂) = "(" ++ show t₁ ++ " " ++ show t₂ ++ ")"

data Kind
    = Star
    | KindArrow Kind Kind
        deriving (Eq)
instance Show Kind where
    show Star = "*"
    show (KindArrow k₁ k₂) = "(⇒ " ++ show k₁ ++ " " ++ show k₂ ++ ")"

data Variable
    = Var String
        deriving (Eq)
instance Show Variable where
    show (Var s) = s 

data Token
    = TokenTrue
    | TokenFalse
    | TokenIf
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
    | TokenLambda
    | TokenBigLambda
    | TokenArrow
    | TokenForall
    | TokenVar String
    | TokenTypeInt
    | TokenTypeBool
    | TokenKind
    | TokenDoubleArrow
    | TokenDefine
        deriving (Show)

-- |Turns a string into the tokens we have defined.
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
-- Comment
lexer (';':cs) = lexer $ eatTillNewLine cs
    where
        eatTillNewLine :: String -> String
        eatTillNewLine [] = []
        eatTillNewLine ('\n':cs) = cs
        eatTillNewLine (_:cs) = eatTillNewLine cs
lexer ('0':cs) = TokenZero : lexer cs
lexer ('(':cs) = TokenOpenBracket : lexer cs
lexer (')':cs) = TokenCloseBracket : lexer cs
lexer ('[':cs) = TokenOpenSqBracket : lexer cs
lexer (']':cs) = TokenCloseSqBracket : lexer cs
lexer ('λ':cs) = TokenLambda : lexer cs
lexer ('Λ':cs) = TokenBigLambda : lexer cs
lexer ('∀':cs) = TokenForall : lexer cs
lexer ('→':cs) = TokenArrow : lexer cs
lexer ('⇒':cs) = TokenDoubleArrow : lexer cs
lexer ('*':cs) = TokenKind : lexer cs
lexer cs =
    case span isAlpha cs of
        ("if", rest) -> TokenIf : lexer rest
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
        ("int", rest) -> TokenTypeInt : lexer rest
        ("bool", rest) -> TokenTypeBool : lexer rest
        ("define", rest) -> TokenDefine : lexer rest
        (var, rest) -> TokenVar var : lexer rest

}
