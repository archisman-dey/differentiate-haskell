-- import necessary modules
import Data.Char (isSpace)
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr
import Text.Parsec.Language ( emptyDef )


-- define the grammar
data Expr = Var Char
            | Const Double
            | BinExp BinaryOp Expr Expr
            | UnExp UnaryOp Expr
            deriving Eq

data BinaryOp = Add | Sub | Mul | Div | Pow deriving Eq

data UnaryOp = Minus | Sin | Cos | Tan | Sec | Log | Exp | Sqrt deriving Eq


-- nicely show Expr
instance Show Expr where
    show (Var a) = "(" ++ show a ++ ")"
    show (Const a) = "(" ++ show a ++ ")"
    show (BinExp a b c) = "(" ++ show b ++ show a ++ show c ++ ")"
    show (UnExp a b) = "(" ++ show a ++ show b ++ ")"

instance Show BinaryOp where
    show Add = " + "
    show Sub = " - "
    show Mul = " * "
    show Div = " / "
    show Pow = " ^ "

instance Show UnaryOp where
    show Minus = "-"
    show Sin = "sin"
    show Cos = "cos"
    show Tan = "tan"
    show Sec = "sec"
    show Log = "log"
    show Exp = "exp"
    show Sqrt = "sqrt"


-- function to simplify expressions using pattern matching
simplify :: Expr -> Expr
-- simplification rules for addition
simplify (BinExp Add (Const a) (Const b)) = Const (a + b)
simplify (BinExp Add a (Const 0)) = simplify a
simplify (BinExp Add (Const 0) a) = simplify a
simplify (BinExp Add a b) = BinExp Add (simplify a) (simplify b)

-- simplification rules for subtraction
simplify (BinExp Sub (Const a) (Const b)) = Const (a - b)
simplify (BinExp Sub a (Const 0)) = simplify a
simplify (BinExp Sub (Const 0) a) = simplify (UnExp Minus a)
simplify (BinExp Sub a b) = BinExp Sub (simplify a) (simplify b)

-- simplification rules for multiplication
simplify (BinExp Mul (Const a) (Const b)) = Const (a * b)
simplify (BinExp Mul (Const 0) a) = Const 0
simplify (BinExp Mul a (Const 0)) = Const 0
simplify (BinExp Mul (Const 1) a) = simplify a
simplify (BinExp Mul a (Const 1)) = simplify a
simplify (BinExp Mul (Const (-1)) a) = simplify (UnExp Minus a)
simplify (BinExp Mul a (Const (-1))) = simplify (UnExp Minus a)
simplify (BinExp Mul (Const a) (BinExp Mul (Const b) c)) = BinExp Mul (Const (a * b)) c
simplify (BinExp Mul (Const a) (BinExp Mul b (Const c))) = BinExp Mul (Const (a * c)) b
simplify (BinExp Mul a b) = BinExp Mul (simplify a) (simplify b)

-- simplification rules for division
simplify (BinExp Div (Const 0) a) = Const 0
simplify (BinExp Div a (Const 0)) = error "Division by Zero!"
simplify (BinExp Div (Const a) (Const b)) = Const (a / b)
simplify (BinExp Div a (Const 1)) = simplify a
simplify (BinExp Div a (Const (-1))) = simplify (UnExp Minus a)
simplify (BinExp Div a b) = BinExp Div (simplify a) (simplify b)

-- simplification rules for power
simplify (BinExp Pow (Const a) (Const b)) = Const (a ** b)
simplify (BinExp Pow a (Const 0)) = Const 1
simplify (BinExp Pow a (Const 1)) = simplify a
simplify (BinExp Pow (BinExp Pow a (Const b)) (Const c)) = BinExp Pow (simplify a) (Const (b * c))
simplify (BinExp Pow a b) = BinExp Pow (simplify a) (simplify b)

-- simplification rules for minus
simplify (UnExp Minus (Const a)) = Const (-a)
simplify (UnExp Minus (UnExp Minus a)) = simplify a
simplify (UnExp Minus (BinExp Add a b)) = BinExp Add (UnExp Minus (simplify a)) (UnExp Minus (simplify b))
simplify (UnExp Minus (BinExp Sub a b)) = BinExp Sub (simplify b) (simplify a)
simplify (UnExp Minus (BinExp Mul a b)) = BinExp Mul (UnExp Minus (simplify a)) (simplify b)
simplify (UnExp Minus (BinExp Div a b)) = BinExp Div (UnExp Minus (simplify a)) (simplify b)
simplify (UnExp Minus a) = UnExp Minus (simplify a)

-- default simplification rule
simplify x = x


-- function to simplify repeatedly until expression remains unchanged
simplifyFull :: Expr -> Expr
simplifyFull a = simplifyFull' a (Const 0) where
    simplifyFull' curr last | curr == last = curr
                            | otherwise = simplifyFull' curr' curr where
                                curr' = simplify curr

-- function to differentiate using pattern matching
diff :: Expr -> Expr
diff (Const a) = Const 0
diff (Var a) = Const 1
diff (BinExp Add a b) = BinExp Add (diff a) (diff b)
diff (BinExp Sub a b) = BinExp Sub (diff a) (diff b)
diff (BinExp Mul a b) = BinExp Add (BinExp Mul a (diff b)) (BinExp Mul (diff a) b)
diff (BinExp Div a b) = BinExp Div (BinExp Sub (BinExp Mul (diff a) b) (BinExp Mul a (diff b))) (BinExp Pow b (Const 2))
diff (BinExp Pow a (Const b)) = BinExp Mul (Const b) (BinExp Pow a (BinExp Sub (Const b) (Const 1)))
diff (BinExp Pow a b) = error "differentiating non-constant powers unsupported"
diff (UnExp Minus a) = UnExp Minus (diff a)
diff (UnExp Sin a) = BinExp Mul (UnExp Cos a) (diff a)
diff (UnExp Cos a) = BinExp Mul (UnExp Minus (UnExp Sin a)) (diff a)
diff (UnExp Tan a) = BinExp Mul (BinExp Pow (UnExp Sec a) (Const 2)) (diff a)
diff (UnExp Sec a) = BinExp Mul (BinExp Mul (UnExp Sec a) (UnExp Tan a)) (diff a)
diff (UnExp Log a) = BinExp Mul (BinExp Div (Const 1) a) (diff a)
diff (UnExp Exp a) = BinExp Mul (UnExp Exp a) (diff a)
diff (UnExp Sqrt a) = BinExp Mul (BinExp Div (Const 0.5) (UnExp Sqrt a)) (diff a)


-- generate Lexer
def = emptyDef {
    T.opLetter = oneOf "+-*/^~nscgpt",
    T.reservedOpNames = ["+", "-", "*", "/", "^", "~", "sin", "cos", "tan", "sec", "log", "exp", "sqrt"]
}

lexer = T.makeTokenParser def

op = T.reservedOp lexer
float = T.float lexer
parens = T.parens lexer

-- build the parser
exprParser = buildExpressionParser opTable expr

opTable = [ [Infix  (op "^"     >> return (BinExp Pow)) AssocRight  ],
            [Infix  (op "/"     >> return (BinExp Div)) AssocLeft   ],
            [Infix  (op "*"     >> return (BinExp Mul)) AssocLeft   ],
            [Infix  (op "-"     >> return (BinExp Sub)) AssocLeft   ],
            [Infix  (op "+"     >> return (BinExp Add)) AssocLeft   ],
            [Prefix (op "~"     >> return (UnExp Minus))            ],
            [Prefix (op "sin"   >> return (UnExp Sin))              ],
            [Prefix (op "cos"   >> return (UnExp Cos))              ],
            [Prefix (op "tan"   >> return (UnExp Tan))              ],
            [Prefix (op "sec"   >> return (UnExp Sec))              ],
            [Prefix (op "log"   >> return (UnExp Log))              ],
            [Prefix (op "exp"   >> return (UnExp Exp))              ],
            [Prefix (op "sqrt"  >> return (UnExp Sqrt))             ]]

expr =  fmap Var (char 'x')
    <|> fmap Const float
    <|> parens exprParser


-- main
main = do
    putStrLn "Expression in terms of x? (Note: Use ~ for unary minus)"
    input <- getLine
    case parse exprParser "" (filter (not . isSpace) input) of
        Left e -> print e
        Right r -> do
            putStrLn ("Parsed as: " ++ show r)
            putStrLn ("Result: " ++ show (simplifyFull $ diff r))
