module Parser
  ( parseExpr
  , parseBind
  , parseType
  , parseDecl
  , parseStdin
  , parseSourceFile
  ) where

import Data.Functor
import Data.Text.Lazy (Text)

import Text.Parsec
import Text.Parsec.Text.Lazy

import Lexer
import Syntax

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
  <|> sigD
  <|> infixD
  <?> "top-level declaration"

-- | Top level bindings
bindD :: Parser Decl
bindD = BindD <$> bind

-- | Type signatures
sigD :: Parser Decl
sigD = do
  sig_
  v <- identifier
  colon_
  t <- type'
  return (SigD (Var v) t)

-- | Infix operators
infixD :: Parser Decl
infixD = do
  f <- fixity
  prec <- decimalLit
  whiteSpace
  op <- operator
  equal_
  v <- identifier
  return (InfixD f prec (Var op) (Var v))

fixity :: Parser Fixity
fixity = (infixl_ $> L) <|> (infixr_ $> R) <|> (infix_ $> None)

----------------------------------------
-- | Binds
----------------------------------------

bind :: Parser Bind
bind =
  valB
  <|> funB
  <?> "bind"

-- | Value binds
valB :: Parser Bind
valB = do
  val_
  v <- identifier
  equal_
  e <- expr
  return (ValB (Var v) e)

-- | Function binds with implicit fixpoints
funB :: Parser Bind
funB = do
  fun_
  f <- identifier
  args <- many1 identifier
  equal_
  e <- expr
  return (FunB (Var f) (Var <$> args) e)

----------------------------------------
-- | Expressions
----------------------------------------

expr :: Parser Expr
expr =
  try asE
  <|> nonAsE
  <?> "expression"

nonAsE :: Parser Expr
nonAsE =
  try infixE
  <|> try appE
  <|> letE
  <|> caseE
  <|> ifE
  <|> lamE
  <|> fixE
  <|> sumE
  <?> "expression"

-- | Type annotated expressions
asE :: Parser Expr
asE = do
  e <- nonAsE
  colon_
  t <- type'
  return (AsE e t)

-- | Infix operators
infixE :: Parser Expr
infixE =
  customInfix
  <?> "infix operator"

customInfix :: Parser Expr
customInfix = do
  e1 <- appE
  op <- operator
  e2 <- appE
  return (InfixE (Var op) e1 e2)

-- | Applied expressions without type annotations
appE :: Parser Expr
appE = chainl1 atomE (optional whiteSpace $> AppE)

-- | Atomic expressions
atomE :: Parser Expr
atomE =
  varE
  <|> litE
  <|> try (parens expr)
  <|> tupE
  <?> "atom"

-- | Literals
litE :: Parser Expr
litE = LitE <$> literal

-- | Variables
varE :: Parser Expr
varE = VarE . Var <$> identifier

-- | Lambda abstractions
lamE :: Parser Expr
lamE = do
  fn_
  vs <- many1 identifier
  darrow_
  e <- expr
  return (foldr LamE e (Var <$> vs))

-- | Tuple expressions
-- | Note: unit expression is represented as (TupT [])
tupE :: Parser Expr
tupE = TupE <$> parens (expr `sepBy` comma_)

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
varP = VarP . Var <$> identifier

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
  <|> conT
  <|> try (parens type')
  <|> tupT
  <?> "non-sum type"

-- | Type variables
varT :: Parser Type
varT = VarT . TVar <$> identifier

-- | Type constructors
conT :: Parser Type
conT = ConT . Var <$> identifierT

-- | Product types
-- | Note: Unit type is represented as (TupT [])
tupT :: Parser Type
tupT = TupT <$> parens (type' `sepBy` comma_)

----------------------------------------
-- | Parsing functions
----------------------------------------

-- | Parse a single expression
parseExpr :: Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<interactive>"

-- | Parse a type
parseType :: Text -> Either ParseError Type
parseType = parse (contents type') "<interactive>"

-- | Parse a expression bind
parseBind :: Text -> Either ParseError Bind
parseBind = parse (contents bind) "<interactive>"

-- | Parse a top level declaration
parseDecl :: Text -> Either ParseError Decl
parseDecl = parse (contents decl) "<interactive>"

-- | Parse the interactive input, either expression or declaration
parseStdin :: Text -> Either ParseError (Either Expr Decl)
parseStdin = parse (contents stdin) "<interactive>"

-- | Parse all top level binds in a source file
parseSourceFile :: FilePath -> Text -> Either ParseError [Decl]
parseSourceFile = parse (contents (many decl))
