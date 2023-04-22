module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Data.Char

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

-- BNF for long lambda
-- <longLambdaP> ::= <longLambda>
-- <longLambda> ::= "("  <longParam> <longExpr> ")"
-- <longParam> ::= "λ" <lambdaVar> "."
-- <longExpr> ::= <longExprCont> | <longLambda> 
-- <longExprCont> ::= <longExprList>
-- <longExprList> ::= <singleExpr> <longExprList> | <longExprBrac> <longExprList> | <singleExpr> | <longExprBrac>
-- <singleExpr> ::= "term" <lambdaVar>
-- <longExprBrac> ::= "(" <longExprCont> ")"

longLambdaP :: Parser Lambda
longLambdaP = do
    lambda <- longLambda
    pure (build $ lambda) 

-- For a single lambda expression
-- Parameters are on the left hand side of the '.' - longParam
-- Expressions are the right hand side of the '.' - longExpr

-- For a single lambda expression in the form '(λparameter.expression)'
longLambda :: Parser Builder
longLambda =  do
    is '('
    param <- longParam -- λparameter.
    expr <- longExpr -- expression
    is ')'
    pure (param expr) 

-- Parse a parameter of long lambda expression which is in the form of 'λparameter.', 
-- the parameter is return by calling function lambdaVar
longParam :: Parser (Builder -> Builder)
longParam = do
    is 'λ'
    var <- lambdaVar -- single parameter
    is '.'
    pure (lam var)

-- The right hand side of '.' can be either function body without another lambda, or function body with another lambda
longExpr :: Parser Builder
longExpr = longExprCont ||| longLambda

-- Parse multiple variables of function body by calling longExprList function
longExprCont :: Parser Builder
longExprCont = do
    l <- longExprList 
    pure (foldl (ap) (head l) (tail l))

-- Recursive function which returns a list of 
-- single variable using function singleExpr and 
-- variables with bracket using longExprBrac
longExprList :: Parser [Builder]
longExprList = do
    p <- singleExpr ||| longExprBrac -- 2 types of function body
    l <- longExprList ||| pure ([]) -- Recursively call itself, base case is empty list
    pure (p:l)

-- Parse a single variable (term) by calling lambdaVar
singleExpr :: Parser Builder
singleExpr = do
    var <- lambdaVar 
    pure (term var)

-- Parse variables within bracket
longExprBrac :: Parser Builder
longExprBrac = do
    is '('
    expr <- longExprCont
    is ')'
    pure ((expr))

-- Parse Char of lower case
lambdaVar :: Parser Char
lambdaVar = satisfy isLower


-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- BNF for short lambda
-- <shortLambdaP> ::= <lamList> 
-- <lamList> ::= <shortLambda1><lamList> | <shortLambda2><lamList> | <shortLambda1> | <shortLambda2>
-- <shortLambda1> ::= "λ" <shortParam> "." <shortExpr> 
-- <shortLambda2> ::= "(" "λ" <shortParam> "." <shortExpr> ")"
-- <shortParam> ::= <shortParamList> 
-- <shortParamList> ::= <singleParam> | <singleParam> <shortParamList>
-- <shortExpr> ::= <shortExprList>
-- <shortExprList> ::= <singleExpr><shortExprList> | <shortExprBrac><shortExprList> | <shortLambda2><shortExprList> | <singleExpr> | <shortExprBrac> | <shortLambda2>
-- <singleExpr> ::= "term" <lambdaVar>
-- <shortExprBrac> ::= "(" <shortExpr> ")"

shortLambdaP :: Parser Lambda
shortLambdaP = do
    a <- lamList
    pure (build $ foldl (ap) (head a) (tail a))

-- Recursive functions creates a list of 2 possible type of Lambda equation (shortLambda2 or shortLambda1) 
lamList :: Parser [Builder]
lamList = do
    -- (/x.xx)/y.yy
    a <- shortLambda2 ||| shortLambda1
    b <- lamList ||| pure ([])
    pure (a:b)

-- For a single lambda expression
-- Parameters are on the left hand side of the '.' - shortParam
-- Expressions are the right hand side of the '.' - shortExpr

-- lambda without brackets, in the form of 'λparameters.expression'
shortLambda1 :: Parser Builder
shortLambda1 = do
    is 'λ'
    param <- shortParam -- parameters
    is '.'
    expr <- shortExpr -- expressions
    pure (param expr) 

-- lambda with brackets, in the form of '(λparameters.expression)'
shortLambda2 :: Parser Builder
shortLambda2 = do
    is '('
    is 'λ'
    param <- shortParam -- parameters
    is '.'
    expr <- shortExpr -- expressions
    is ')'
    pure (param expr)

-- Parse multiple parameters (lam) of lambda calling shortParamList function
shortParam :: Parser (Builder -> Builder)
shortParam = do
    l <- shortParamList
    pure (foldl (<$>) (head l) (tail l))

-- Recursive function which returns a list of parameters using function singleParam  
shortParamList :: Parser [(Builder -> Builder)]
shortParamList = do
    p <- singleParam -- returns a single parameter (lam)
    l <- shortParamList ||| pure ([]) -- Recursively call itself, base case is empty list
    pure (p:l)

-- Function which calls lambdaVar, produce a single parameter using 'lam'
singleParam :: Parser (Builder -> Builder)
singleParam = do
    var <- lambdaVar 
    pure (lam var)

-- Parse function body by calling shortExprList function
shortExpr :: Parser Builder
shortExpr = do
    l <- shortExprList
    pure (foldl (ap) (head l) (tail l))

-- Recursive function which returns list of 3 type of expressions: 
-- Expressions with brackets, expression which is another lambda, single variable
shortExprList :: Parser [Builder]
shortExprList = do
    p <- shortExprBrac ||| shortLambda1 ||| singleExpr
    l <- shortExprList ||| pure ([])
    pure (p:l)

-- Parse expressions (right hand side of lambda) with brackets
-- For example, λxy.xy(xx) or λx.x(λy.yy)
shortExprBrac :: Parser Builder
shortExprBrac = do
    is '('
    expr <- shortExpr
    is ')'
    pure ((expr))

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

-- Parse either lambda in long lambda form or short lambda form
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP 

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

logicP :: Parser Lambda
logicP = do
    l <- boolExpr
    pure (build $ l)

-- Functions that parse the correct order of operations (“()” -> “not” -> “and” -> “or”) using chain function
boolExpr :: Parser Builder
boolExpr = exprIf ||| chain exprAnd opOr

exprAnd :: Parser Builder
exprAnd = chain (complexCalc ||| boolP ||| exprBrac) opAnd

boolP :: Parser Builder
boolP = exprNot ||| trueP ||| falseP

-- Parser used in chain for 'or' operator
-- It check for 'or' using isOr 
-- It returns the parser for 'or' using or'
opOr :: Parser (Builder -> Builder -> Builder)
opOr = isOr >> pure or'

or' :: Builder -> Builder -> Builder
or' a b = (('x' `lam` 'y' `lam` if' `ap` term 'x' `ap` true' `ap` term 'y') `ap` a `ap` b)

-- Parser used in chain for 'and' operator
-- It check for 'and' using isAnd
-- It returns the parser for 'and' using and'
opAnd :: Parser (Builder -> Builder -> Builder)
opAnd = isAnd >> pure and'

and' :: Builder -> Builder -> Builder
and' a b = (('x' `lam` 'y' `lam` if' `ap` term 'x' `ap` term 'y' `ap` false') `ap` a `ap` b)

-- Parser combinator which parse expressions with Bracket
exprBrac :: Parser Builder
exprBrac = do
    spaces
    is '('
    spaces
    p <- boolExpr
    spaces
    is ')'
    spaces
    pure (p)

-- Parser combinator which parse expressions with 'if' 'then' 'else'
exprIf :: Parser Builder
exprIf = do
    a <- ifP
    spaces
    b <- (boolExpr)
    spaces
    string "then"
    spaces
    t <- (boolExpr)
    spaces
    string "else"
    spaces
    f <- (boolExpr) 
    spaces
    pure (a `ap` b `ap` t `ap` f)

-- Parser combinator which parse expressions with 'not'
exprNot :: Parser Builder
exprNot = do
    n <- notP
    spaces
    a <- boolExpr
    spaces
    pure (n `ap` a)

-- Parser for 'True'
trueP :: Parser Builder
trueP = isTrue >> pure true'

true' :: Builder
true' = (boolToLam True)

-- Parser for 'False'
falseP :: Parser Builder
falseP = isFalse >> pure false'

false' :: Builder
false' = (boolToLam False)

-- Parser for 'if'
ifP :: Parser Builder
ifP = isIf >> pure if'

if' :: Builder
if' = ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f')

-- Parser for 'not'
notP :: Parser Builder
notP = isNot >> pure not'

not' :: Builder
not' = ('x' `lam` if' `ap` term 'x' `ap` false' `ap` true')

-- Functions which check for 'and'
isAnd :: Parser String
isAnd = do
    spaces
    string "and"
    spaces

-- Functions which check for 'or'
isOr :: Parser String
isOr = do
    spaces
    string "or"
    spaces

-- Functions which check for 'not'
isNot :: Parser String
isNot = do
    spaces
    string "not"
    spaces

-- Functions which check for 'if'
isIf :: Parser String
isIf = do
    spaces
    string "if"
    spaces

-- Functions which check for 'True'
isTrue :: Parser String
isTrue = do
    spaces
    string "True"
    spaces

-- Functions which check for 'False'
isFalse :: Parser String
isFalse = do
    spaces
    string "False"
    spaces
    

-- not True and (True and False) 
-- True and False or True
-- "not True and (True and False) and False"
-- and (not True) ((True and False) and False)
-- and (not True) (and (True and False) (False))
-- and (False) (False)
---"False and True or False"
-- or (False and True) (False)

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

-- Function for chain
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a

basicArithmeticP :: Parser Lambda
basicArithmeticP = do
    n <- exprBa
    pure (build $ n)

-- Uses chain for repeated operators '+', '-'
exprBa :: Parser Builder
exprBa = chain numP add

-- Parser use in chain for Addition and Subtraction
-- It check for symbol '+' and '-' using isPlus and isMinus
-- It returns the parser for '+' or '-' using plusP and minusP
add :: Parser (Builder -> Builder -> Builder)
add = (isPlus >> pure plusP) ||| (isMinus >> pure minusP)

-- Parser for plus
plusP :: Builder -> Builder -> Builder
plusP a b = (('x' `lam` 'y' `lam` term 'y' `ap` succ' `ap` term 'x') `ap` a `ap` b)

succ' :: Builder
succ' = ('n' `lam` 'f' `lam` 'x' `lam` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))

-- Parser for minus
minusP :: Builder -> Builder -> Builder
minusP a b = (('x' `lam` 'y' `lam` term 'y' `ap` pred' `ap` term 'x')) `ap` a `ap` b

pred' :: Builder
pred' = ('n' `lam` 'f' `lam` 'x' `lam` term 'n' `ap` ('g' `lam` 'h' `lam` term 'h' `ap` (term 'g' `ap` term 'f')) `ap` ('u' `lam` term 'x') `ap` ('u' `lam` term 'u'))

-- Function check for symbol '+' 
isPlus :: Parser String
isPlus = do
    spaces
    is '+'
    spaces

-- Function check for symbol '-' 
isMinus :: Parser String
isMinus = do
    spaces
    is '-'
    spaces
    
-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

arithmeticP :: Parser Lambda
arithmeticP = do
    n <- exprC
    pure (build $ n)

-- Uses chain for repeated chains of operators '+', '-', '*', '**'
-- '**' operator has highest precendence
exprA :: Parser Builder
exprA = chain (numP ||| bracP) expo

-- '*' operator has highest precendence
exprB :: Parser Builder
exprB = chain exprA times

-- '+' and '-' operator has lowest precedence
exprC :: Parser Builder
exprC = chain exprB add

-- Parser use in chain for Exponent
-- It check for symbol '**' using isExpo 
-- It returns the parser for '**' using expoP
expo :: Parser (Builder -> Builder -> Builder)
expo = isExpo >> pure expoP

expoP :: Builder -> Builder -> Builder
expoP a b = (('x' `lam` 'y' `lam` term 'y' `ap` term 'x') `ap` a `ap` b)

-- Parser use in chain for Multiplication
-- It check for symbol '*' using isMult 
-- It returns the parser for '*' using timesP
times :: Parser (Builder -> Builder -> Builder)
times = isMult >> pure timesP

timesP :: Builder -> Builder -> Builder
timesP a b = (('x' `lam` 'y' `lam` 'f' `lam` term 'x' `ap` (term 'y' `ap` term 'f')) `ap` a `ap` b)

-- Function check for symbol '**' 
isExpo :: Parser String
isExpo = do
    spaces
    string "**"
    spaces

-- Function check for symbol '*' 
isMult :: Parser String
isMult = do
    spaces
    is '*'
    spaces

-- Parser for brackets
bracP :: Parser Builder
bracP = do
    spaces
    is '('
    spaces
    n <- exprC
    is ')'
    spaces
    pure (n)

-- Parser for number
numP :: Parser Builder
numP = do
    l <- read <$> digitList
    pure (intToLam $ l)

digitList :: Parser [Char]
digitList = do
    p <- digitP
    l <- digitList ||| pure ([])
    pure (p:l)

digitP :: Parser Char
digitP = do 
    a <- satisfy isDigit
    pure (a)



-- -- | Exercise 3

-- -- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- -- | x <= y = LEQ = λmn.isZero (minus m n)
-- -- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- -- | The helper function you'll need is:
-- -- | isZero = λn.n(λx.False)True

-- -- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- -- Result >< Just True
-- --
-- -- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- -- Result >< Just False

complexCalcP :: Parser Lambda
complexCalcP = do
    n <- boolExpr -- in boolean section
    pure (build $ n)

-- Helper function
isZero' :: Builder
isZero' = ('n' `lam` term 'n' `ap` ('x' `lam` boolToLam False) `ap` boolToLam True)

-- chain it with artihmetic's exprC
complexCalc :: Parser Builder
complexCalc = chain exprC compOp

compOp :: Parser (Builder -> Builder -> Builder)
compOp = meq ||| leq ||| eq ||| neq ||| moreThan ||| lessThan

-- Parser for '>'
moreThan :: Parser (Builder -> Builder -> Builder)
moreThan = isMoreThan >> pure more

more :: Builder -> Builder -> Builder
more a b = (('x' `lam` if' `ap` term 'x' `ap` false' `ap` true') `ap` lessThanEq a b)

-- Parser for '<'
lessThan :: Parser (Builder -> Builder -> Builder)
lessThan = isLessThan >> pure less

less :: Builder -> Builder -> Builder
less a b = (('x' `lam` if' `ap` term 'x' `ap` false' `ap` true') `ap` moreThanEq a b)

-- Parser for '>='
meq :: Parser (Builder -> Builder -> Builder)
meq = isMoreThanEq >> pure moreThanEq

moreThanEq :: Builder -> Builder -> Builder
moreThanEq a b = (or' (more a b) (equal a b)) 

-- Parser for '<='
leq :: Parser (Builder -> Builder -> Builder)
leq = isLessThanEq >> pure lessThanEq

lessThanEq :: Builder -> Builder -> Builder
lessThanEq a b = (('m' `lam` 'n' `lam` isZero' `ap` (minusP (term 'm') (term 'n'))) `ap` a `ap` b)

-- Parser for '=='
eq :: Parser (Builder -> Builder -> Builder)
eq = isEqual >> pure equal

equal :: Builder -> Builder -> Builder
equal a b = (('m' `lam` 'n' `lam` and' (lessThanEq (term 'm') (term 'n')) (lessThanEq (term 'n') (term 'm'))) `ap` a `ap` b)

-- Parser for '!='
neq :: Parser (Builder -> Builder -> Builder)
neq = isNotEqual >> pure notEqual

notEqual :: Builder -> Builder -> Builder
notEqual a b = (('x' `lam` if' `ap` term 'x' `ap` false' `ap` true') `ap` equal a b)

-- Functions which check for '>' symbol
isMoreThan :: Parser String
isMoreThan = do
    spaces
    is '>'
    spaces

-- Functions which check for '<' symbol
isLessThan :: Parser String
isLessThan = do
    spaces
    is '<'
    spaces

-- Functions which check for '>=' symbol
isMoreThanEq :: Parser String
isMoreThanEq = do
    spaces
    string ">="
    spaces

-- Functions which check for '<=' symbol
isLessThanEq :: Parser String
isLessThanEq = do
    spaces
    string "<="
    spaces

-- Functions which check for '==' symbol
isEqual :: Parser String
isEqual = do
    spaces
    string "=="
    spaces

-- Functions which check for '!=' symbol
isNotEqual :: Parser String
isNotEqual = do
    spaces
    string "!="
    spaces


-- {-|
--     Part 3
-- -}

-- -- | Exercise 1

-- -- | The church encoding for list constructs are given below
-- -- | [] = null = λcn.n
-- -- | isNull = λl.l(λht.False) True
-- -- | cons = λhtcn.ch(tcn)
-- -- | head = λl.l(λht.h) False
-- -- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
-- --
-- -- >>> parse listP "[]"
-- -- Result >< \cn.n
-- --
-- -- >>> parse listP "[True]"
-- -- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
-- --
-- -- >>> parse listP "[0, 0]"
-- -- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
-- --
-- -- >>> parse listP "[0, 0"
-- -- UnexpectedEof
-- listP :: Parser Lambda
-- listP = undefined

listP :: Parser Lambda
listP = emptyP ||| (do
    is '['
    n <- elementP
    l <- list elementContP
    is ']'
    pure (build $ n `ap` foldr (ap) (('c' `lam` 'n' `lam` term 'n')) (l) ))

emptyP :: Parser Lambda
emptyP = do
    is '['
    is ']'
    pure (build $ 'c' `lam` 'n' `lam` term 'n')

-- First element of the list
elementP :: Parser Builder
elementP = do
    spaces
    n <- boolExpr
    spaces
    pure (cons `ap` n)

cons :: Builder
cons = ('h' `lam` 't' `lam` 'c' `lam` 'n' `lam` term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n'))

-- Subsequent element of the list
elementContP :: Parser Builder
elementContP = do
    spaces
    is ','
    spaces
    n <- boolExpr
    spaces
    pure (cons `ap` n)

-- -- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- -- Result >< Just True
-- --
-- -- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- -- Result >< Just False
-- --
-- -- >>> lamToBool <$> parse listOpP "isNull []"
-- -- Result >< Just True
-- --
-- -- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- -- Result >< Just False

-- List operators parser
listOpP :: Parser Lambda
listOpP = do
    p <- mapP ||| foldrP ||| nullP ||| headP ||| tailP ||| isNullP ||| consP
    pure (build $ p)

-- Parser for 'null'
nullP :: Parser Builder
nullP = do
    spaces
    string "null"
    pure ('c' `lam` 'n' `lam` term 'n')

-- Parser for 'head'
headP :: Parser Builder
headP = do
    spaces
    string "head"
    spaces
    l <- listP ||| listOpP
    pure (('l' `lam` term 'l' `ap` ('h' `lam` 't' `lam` term 'h') `ap` false') `ap` lamToBuilder l)

-- Parser for 'tail' or 'rest'
tailP :: Parser Builder
tailP = do
    spaces
    string "tail" ||| string "rest"
    spaces
    l <- listP ||| listOpP
    pure (('l' `lam` 'c' `lam` 'n' `lam` term 'l' `ap` ('h' `lam` 't' `lam` 'g' `lam` term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c')) `ap` ('t' `lam` term 'n') `ap` ('h' `lam` 't' `lam` term 't')) `ap` lamToBuilder l)

-- Parser for 'isNull'
isNullP :: Parser Builder
isNullP = do
    spaces
    string "isNull"
    spaces
    l <- listP ||| listOpP
    pure (('l' `lam` term 'l' `ap` ('h' `lam` 't' `lam` false') `ap` true') `ap` lamToBuilder l)

-- Parser for 'cons'
consP :: Parser Builder
consP = do
    spaces
    string "cons"
    spaces
    p <- boolExpr 
    spaces
    l <- listP ||| listOpP
    pure (cons `ap` p `ap` lamToBuilder l)



-- -- | Exercise 2

-- -- | Implement your function(s) of choice below!

-- Parser for 'map'
mapP :: Parser Builder
mapP = do
    spaces
    string "map"
    spaces
    -- For the function to apply the list element
    is '('
    spaces
    f <- expo ||| times ||| add ||| compOp ||| opAnd ||| opOr -- Alternatives of possible function such as + and *
    spaces
    a <- boolExpr -- Expressions such as number, True, False
    spaces
    is ')'
    spaces
    -- For the list
    is '['
    n <- boolExpr -- Fist element of the list
    let m = f n a -- Apply function to first element of list
    l <- mapList f a -- Apply function to each element of list
    is ']'
    p <- pure (fmap (ap cons) (m:l))
    pure (foldr (ap) (('c' `lam` 'n' `lam` term 'n')) (p) )

-- Function use in mapP to apply input function to subsequent element of list
mapList :: (Builder -> Builder -> Builder) -> Builder -> Parser [Builder]
mapList f a = do
    spaces
    is ','
    p <- boolExpr -- element of list
    let n = f p a -- Apply function to the element
    l <- mapList f a ||| pure ([]) -- Recursive for subsequent elements in list
    pure (n:l)

-- Parser for 'foldr'
foldrP :: Parser Builder
foldrP = do
    spaces
    string "foldr"
    spaces
    is '('
    spaces
    -- For the function to apply the list element
    f <- expo ||| times ||| add ||| compOp ||| opAnd ||| opOr -- Alternatives of possible function such as + and *
    spaces
    is ')'
    spaces
    is '('
    spaces
    a <- boolExpr -- Expressions such as number, True, False
    spaces
    is ')'
    spaces
    -- For the list
    is '['
    n <- boolExpr -- parse the first element of list
    l <- foldrList-- parse the rest of the element of list
    is ']'
    pure (foldr (f) (a) (n:l)) -- foldr the list

-- Function use in foldrP to create list of elements
foldrList :: Parser [Builder]
foldrList = do
    spaces
    is ','
    p <- boolExpr -- element of list
    l <- foldrList ||| pure ([]) -- Apply function to the element
    pure (p:l)