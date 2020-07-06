module Parser
  ( parseExpr
  , parseBind
  , parseType
  , parseDecl
  , parseStdin
  , parseSourceFile
  , parseModule
  ) where

import System.FilePath

import Control.Monad

import Data.Functor

import Text.Parsec
import Text.Parsec.String

import Lexer
import Var
import Syntax
import Type hiding (ioT)
import Error

----------------------------------------
-- | Parsing functions
----------------------------------------

-- | Parse the interactive input, either expression or declaration
parseStdin :: FilePath -> String -> Either FlimsyError (Either PsExpr PsDecl)
parseStdin = mkParser stdin

-- | Parse a module with its header attached
parseModule :: FilePath -> String -> Either FlimsyError PsModule
parseModule file = mkParser (module' file) file

-- | Parse all top level declarations in a source file
parseSourceFile :: FilePath -> String -> Either FlimsyError [PsDecl]
parseSourceFile = mkParser (many decl)

-- | Parse a single expression
parseExpr :: FilePath -> String -> Either FlimsyError PsExpr
parseExpr = mkParser expr

-- | Parse a type
parseType :: FilePath -> String -> Either FlimsyError Type
parseType = mkParser type'

-- | Parse a expression bind
parseBind :: FilePath -> String -> Either FlimsyError PsBind
parseBind = mkParser bind

-- | Parse a top level declaration
parseDecl :: FilePath -> String -> Either FlimsyError PsDecl
parseDecl = mkParser decl

-- | Build a top-level parser
mkParser :: Parser a -> FilePath -> String -> Either FlimsyError a
mkParser parser path input =
  case parse (contents parser) path input of
    Left err -> Left (ParseError err)
    Right a -> Right a

----------------------------------------
-- | Modules
----------------------------------------

module' :: FilePath -> Parser PsModule
module' file = do
  (name, imps, exps) <- header file
  decls <- many decl
  return Module
    { module_name = name
    , module_path = file
    , module_imports = imps
    , module_exports = exps
    , module_decls = decls
    }

header :: FilePath -> Parser (String, [ModuleName], Maybe [Var])
header file = do
  module_
  name <- identifierU
  when (takeBaseName file /= name) $ do
    unexpected "module name mismatch"
  imports <- optionMaybe (imports_ >> parens (identifierU `sepBy1` comma_))
  exports <- optionMaybe (exports_ >> parens (varOrInfixOp `sepBy1` comma_))
  return (name, maybe [] id imports, exports)

----------------------------------------
-- | Variables
----------------------------------------

var :: Parser Var
var = mkVar <$> identifier

varOrInfixOp :: Parser Var
varOrInfixOp = try infixOp <|> var

infixOp :: Parser Var
infixOp = mkVar <$> parens operator
  <?> "infix operator"

con :: Parser Var
con = mkVar <$> identifierU

----------------------------------------
-- | Stdin (either expression or declaration)
----------------------------------------

stdin :: Parser (Either PsExpr PsDecl)
stdin = (Left <$> try expr) <|> (Right <$> try decl)

----------------------------------------
-- | Top level declarations
----------------------------------------

decl :: Parser PsDecl
decl =
  bindD
  <?> "top-level declaration"

-- | Top level bindings
bindD :: Parser PsDecl
bindD = BindD <$> bind

----------------------------------------
-- | Binds
----------------------------------------

bind :: Parser PsBind
bind =
  try valB
  <|> try infixB
  <|> funB
  <?> "bind"

-- | Value binds
valB :: Parser PsBind
valB = do
  val_
  v <- var
  equal_
  e <- expr
  return (ValB v e)

-- | Function binds with implicit fixpoints
funB :: Parser PsBind
funB = do
  fun_
  f <- var
  args <- many var
  equal_
  e <- expr
  return (FunB f args e)

-- | Infix operators
infixB :: Parser PsBind
infixB = do
  fun_
  f <- parens operator
  arg1 <- var
  arg2 <- var
  equal_
  e <- expr
  return (FunB (mkVar f) [arg1, arg2] e)

----------------------------------------
-- | Expressions
----------------------------------------

expr :: Parser PsExpr
expr =
  try infixE
  <|> try appE
  <|> letE
  <|> caseE
  <|> ifE
  <|> lamE
  <|> fixE
  <|> sumE
  <|> doE
  <?> "expression"

-- | Infix operators
infixE :: Parser PsExpr
infixE = do
  e1 <- try doE <|> appE
  op <- operator
  -- e2 <- appE
  e2 <- expr
  return (InfixE (mkVar op) e1 e2)
  <?> "infix operator"

-- infixOp :: Parser PsExpr
-- infixOp = do

-- | Applied expressions without type annotations
appE :: Parser PsExpr
appE = chainl1 atomE (optional whiteSpace $> AppE)

-- | Atomic expressions
atomE :: Parser PsExpr
atomE =
  varE
  <|> litE
  <|> try (parens expr)
  <|> tupE
  <|> listE
  <?> "atom"

-- | Literals
litE :: Parser PsExpr
litE = LitE <$> literal

-- | Variables
varE :: Parser PsExpr
varE = VarE <$> varOrInfixOp

-- | Lambda abstractions
lamE :: Parser PsExpr
lamE = do
  fn_
  vs <- many1 var
  darrow_
  e <- expr
  return (foldr LamE e vs)

-- | Tuple expressions
-- | Note: unit expression is represented as (TupT [])
tupE :: Parser PsExpr
tupE = TupE <$> parens (expr `sepBy` comma_)

-- | List expressions
listE :: Parser PsExpr
listE = do
  exps <- brackets (expr `sepBy` comma_)
  return (ListE exps)

-- | Sum injections
sumE :: Parser PsExpr
sumE = SumE <$> (left_ *> (Left <$> atomE) <|> right_ *> (Right <$> atomE))

-- | Fixpoint expressions
fixE :: Parser PsExpr
fixE = FixE <$> (fix_ *> atomE)

-- | Let expressions
letE :: Parser PsExpr
letE = do
  let_
  binds <- many1 (bind <* optional semi_)
  in_
  e <- expr
  end_
  return (foldr LetE e binds)

-- | Case expressions
caseE :: Parser PsExpr
caseE = do
  case_
  e <- expr
  of_
  optional pipe_
  alts <- alt `sepBy1` pipe_
  return (CaseE e alts)

-- | If then else expressions
ifE :: Parser PsExpr
ifE = do
  if_
  c <- expr
  then_
  th <- expr
  else_
  el <- expr
  return (IfE c th el)

-- | Do expressions
doE :: Parser PsExpr
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

doS :: Parser PsDoStmt
doS =
  try bindS
  <|> exprS
  <?> "do statement"

bindS :: Parser PsDoStmt
bindS = do
  v <- var
  doarrow_
  e <- expr
  return (BindStmt v e)

exprS :: Parser PsDoStmt
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

alt :: Parser PsAlt
alt = do
  p <- pat
  darrow_
  e <- expr
  return (Alt p e)

----------------------------------------
-- | Patterns
----------------------------------------

pat :: Parser PsPat
pat =
  try litP
  <|> try listP
  <|> try tupP
  <|> try sumP
  <|> try varP
  <|> try wildP
  <?> "pattern"

-- | Literals
litP :: Parser PsPat
litP = LitP <$> literal

-- | Variables
varP :: Parser PsPat
varP = VarP <$> var

-- | Tuples
-- | Note: unit expression is represented as (TupT [])
tupP :: Parser PsPat
tupP = TupP <$> parens (pat `sepBy` comma_)

-- | Sum injections
sumP :: Parser PsPat
sumP = SumP <$> (Left <$> (left_ *> pat) <|>  Right <$> (right_ *> pat))

-- | Wild patterns
wildP :: Parser PsPat
wildP = wild_ $> WildP

-- | List patterns
listP :: Parser PsPat
listP =
  try nilP
  <|> consP
  <?> "list pattern"

nilP :: Parser PsPat
nilP = do
  lbrack_
  rbrack_
  return (ListP NilP)

consP :: Parser PsPat
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
  <|> try conT
  <|> try (parens type')
  <|> tupT
  <|> listT
  <?> "non-sum type"

-- | Type variables
varT :: Parser Type
varT = VarT . TVar <$> identifier

-- | Type constructors
conT :: Parser Type
conT = ConT <$> con

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
