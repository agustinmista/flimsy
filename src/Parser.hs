module Parser
  ( parseExpr
  , parseBind
  , parseType
  , parseDecl
  , parseStdin
  , parseSourceFile
  ) where

import Control.Monad

import Data.Functor
import Data.Text.Lazy (Text)

import Text.Parsec
import Text.Parsec.Text.Lazy

import Lexer
import Syntax

----------------------------------------
-- | Parsing functions
----------------------------------------

-- | Parse the interactive input, either expression or declaration
parseStdin :: FilePath -> Text -> Either ParseError (Either Expr Decl)
parseStdin = parse (contents stdin)

-- | Parse all top level binds in a source file
parseSourceFile :: FilePath -> Text -> Either ParseError [Decl]
parseSourceFile = parse (contents (many decl))

-- | Parse a single expression
parseExpr :: FilePath -> Text -> Either ParseError Expr
parseExpr = parse (contents expr)

-- | Parse a type
parseType :: FilePath -> Text -> Either ParseError Type
parseType = parse (contents type')

-- | Parse a expression bind
parseBind :: FilePath -> Text -> Either ParseError Bind
parseBind = parse (contents bind)

-- | Parse a top level declaration
parseDecl :: FilePath -> Text -> Either ParseError Decl
parseDecl = parse (contents decl)

----------------------------------------
-- | Stdin (either expression or declaration)
----------------------------------------

stdin :: Parser (Either Expr Decl)
stdin = (Left <$> try expr) <|> (Right <$> try decl)

----------------------------------------
-- | Top level declarations
----------------------------------------

decl :: Parser Decl
decl =
  bindD
  <?> "top-level declaration"

-- | Top level bindings
bindD :: Parser Decl
bindD = BindD <$> bind

----------------------------------------
-- | Variables
----------------------------------------

var :: Parser Var
var = Var False <$> identifier

----------------------------------------
-- | Binds
----------------------------------------

bind :: Parser Bind
bind =
  valB
  <|> try infixB
  <|> funB
  <?> "bind"

-- | Value binds
valB :: Parser Bind
valB = do
  val_
  v <- var
  equal_
  e <- expr
  return (ValB v e)

-- | Function binds with implicit fixpoints
funB :: Parser Bind
funB = do
  fun_
  f <- var
  args <- many1 var
  equal_
  e <- expr
  return (FunB f args e)

-- | Infix operators
infixB :: Parser Bind
infixB = do
  fun_
  f <- parens operator
  arg1 <- var
  arg2 <- var
  equal_
  e <- expr
  return (FunB (Var False f) [arg1, arg2] e)

----------------------------------------
-- | Expressions
----------------------------------------

expr :: Parser Expr
expr =
  try infixE
  <|> try appE
  <|> letE
  <|> caseE
  <|> ifE
  <|> lamE
  <|> fixE
  <|> sumE
  <?> "expression"

-- | Infix operators
infixE :: Parser Expr
infixE =
  infixOp
  <?> "infix operator"

infixOp :: Parser Expr
infixOp = do
  e1 <- appE
  op <- operator
  e2 <- appE
  return (InfixE (Var False op) e1 e2)

-- | Applied expressions without type annotations
appE :: Parser Expr
appE = chainl1 atomE (optional whiteSpace $> AppE)

-- | Atomic expressions
atomE :: Parser Expr
atomE =
  try doE
  <|> varE
  <|> litE
  <|> try (parens expr)
  <|> tupE
  <|> listE
  <?> "atom"

-- | Literals
litE :: Parser Expr
litE = LitE <$> literal

-- | Variables
varE :: Parser Expr
varE = VarE <$> var

-- | Lambda abstractions
lamE :: Parser Expr
lamE = do
  fn_
  vs <- many1 var
  darrow_
  e <- expr
  return (foldr LamE e vs)

-- | Tuple expressions
-- | Note: unit expression is represented as (TupT [])
tupE :: Parser Expr
tupE = TupE <$> parens (expr `sepBy` comma_)

-- | List expressions
listE :: Parser Expr
listE = do
  exps <- brackets (expr `sepBy` comma_)
  return (ListE exps)

-- | Sum injections
sumE :: Parser Expr
sumE = SumE <$> (left_ *> (Left <$> atomE) <|> right_ *> (Right <$> atomE))

-- | Fixpoint expressions
fixE :: Parser Expr
fixE = FixE <$> (fix_ *> atomE)

-- | Let expressions
letE :: Parser Expr
letE = do
  let_
  binds <- many1 (bind <* optional semi_)
  in_
  e <- expr
  end_
  return (foldr LetE e binds)

-- | Case expressions
caseE :: Parser Expr
caseE = do
  case_
  e <- expr
  of_
  optional pipe_
  alts <- alt `sepBy1` pipe_
  return (CaseE e alts)

-- | If then else expressions
ifE :: Parser Expr
ifE = do
  if_
  c <- expr
  then_
  th <- expr
  else_
  el <- expr
  return (IfE c th el)

-- | Do expressions
doE :: Parser Expr
doE = do
  do_
  stmts <- braces (doS `sepBy1` semi_)
  let l = last stmts
  let isBindStmt (BindStmt {}) = True
      isBindStmt _             = False
  when (isBindStmt l) $
    unexpected "bind statement"
  return (DoE stmts)

----------------------------------------
-- | Do statements
----------------------------------------

doS :: Parser DoStmt
doS =
  try bindS
  <|> exprS
  <?> "do statement"

bindS :: Parser DoStmt
bindS = do
  v <- var
  doarrow_
  e <- expr
  return (BindStmt v e)

exprS :: Parser DoStmt
exprS = ExprStmt <$> expr

----------------------------------------
-- | Literals
----------------------------------------

literal :: Parser Literal
literal =
  try numberL
  <|> try stringL
  <|> try boolL
  <|> try charL
  <?> "literal"

numberL :: Parser Literal
numberL = either (IntL . fromInteger) DoubleL <$> numberLit

stringL :: Parser Literal
stringL = StringL <$> stringLit

boolL :: Parser Literal
boolL = BoolL <$> (true_ $> True <|> false_ $> False)

charL :: Parser Literal
charL = CharL <$> charLit

----------------------------------------
-- | Case alternatives
----------------------------------------

alt :: Parser Alt
alt = do
  p <- pat
  darrow_
  e <- expr
  return (Alt p e)

----------------------------------------
-- | Patterns
----------------------------------------

pat :: Parser Pat
pat =
  try litP
  <|> try listP
  <|> try tupP
  <|> try sumP
  <|> try varP
  <|> try wildP
  <?> "pattern"

-- | Literals
litP :: Parser Pat
litP = LitP <$> literal

-- | Variables
varP :: Parser Pat
varP = VarP <$> var

-- | Tuples
-- | Note: unit expression is represented as (TupT [])
tupP :: Parser Pat
tupP = TupP <$> parens (pat `sepBy` comma_)

-- | Sum injections
sumP :: Parser Pat
sumP = SumP <$> (Left <$> (left_ *> pat) <|>  Right <$> (right_ *> pat))

-- | Wild patterns
wildP :: Parser Pat
wildP = wild_ $> WildP

-- | List patterns
listP :: Parser Pat
listP =
  try nilP
  <|> consP
  <?> "list pattern"

nilP :: Parser Pat
nilP = do
  lbrack_
  rbrack_
  return (ListP NilP)

consP :: Parser Pat
consP = do
  cons <- brackets $ do
    hds <- pat `sepBy1` comma_
    tl <- optionMaybe (pipe_ >> (varP <|> wildP))
    return (ConsP hds tl)
  return (ListP cons)

----------------------------------------
-- | Types
----------------------------------------

type' :: Parser Type
type' =
  try arrT
  <|> nonArrT
  <?> "type"

-- | Functional types
arrT :: Parser Type
arrT = chainr1 nonArrT (arrow_ $> (:->:))

-- | Non functional types
nonArrT :: Parser Type
nonArrT =
  try sumT
  <|> nonSumT
  <?> "non-function type"

-- | Sum types
sumT :: Parser Type
sumT = do
  t1 <- nonSumT
  plus_
  t2 <- nonSumT
  return (t1 :+: t2)

-- | Non sum types
nonSumT :: Parser Type
nonSumT =
  varT
  <|> try ioT
  <|> conT
  <|> try (parens type')
  <|> tupT
  <|> listT
  <?> "non-sum type"

-- | Type variables
varT :: Parser Type
varT = VarT . TVar <$> identifier

-- | Type constructors
conT :: Parser Type
conT = ConT <$> var

-- | Product types
-- | Note: () is represented as (TupT [])
tupT :: Parser Type
tupT = TupT <$> parens (type' `sepBy` comma_)

-- | List type
listT :: Parser Type
listT = ListT <$> brackets type'

-- | IO type
ioT :: Parser Type
ioT = do
  io_
  t <- type'
  return (IOT t)
