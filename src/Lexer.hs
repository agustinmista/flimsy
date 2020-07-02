{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lexer where

import Data.Text.Lazy

import Data.Functor
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token as Token

langDef = LanguageDef
  { reservedOpNames =
      [ ":", ",", "->", "=>", "|", "=", "_", "[", "]"]
  , reservedNames =
      [ "val", "fun", "sig"
      , "infix", "infixl", "infixr"
      , "let", "in"
      , "if", "then", "else"
      , "case", "of", "end"
      , "fn", "λ", "fix", "left", "right"
      , "true", "false"
      , "do", "IO"
      ]
  , identStart = lower
  , identLetter = alphaNum <|> oneOf "_'"
  , opLetter = oneOf ":!#$%&*+./<=>?@\\^-~"
  , opStart = oneOf ":!#$%&*+./<=>?@\\^-~"
  , commentLine = "#"
  , commentStart = ""
  , commentEnd = ""
  , nestedComments = True
  , caseSensitive = True
  }

-- | The main lexer for the language
lexer :: GenTokenParser Text () Identity
lexer = makeTokenParser langDef

-- | A particular lexer for type identifiers
lexerT :: GenTokenParser Text () Identity
lexerT = makeTokenParser langDef { identStart = upper }

prefix s f = Prefix (reservedOp lexer s >> return f)
binary s f = Infix (reservedOp lexer s >> return f)
contents p = Token.whiteSpace lexer *> p <* eof

identifier  = pack <$> Token.identifier lexer
identifierT = pack <$> Token.identifier lexerT
operator    = pack <$> Token.operator lexer

numberLit  = Token.naturalOrFloat lexer
stringLit  = pack <$> Token.stringLiteral lexer
charLit    = Token.charLiteral lexer
decimalLit = Token.decimal lexer

whiteSpace = Token.whiteSpace lexer
parens     = Token.parens lexer
braces     = Token.braces lexer
brackets   = Token.brackets lexer
commaSep   = Token.commaSep lexer
commaSep1  = Token.commaSep1 lexer
reserved   = Token.reserved lexer
symbol     = Token.symbol lexer


pipe_    = void (Token.symbol lexer "|")
darrow_  = void (Token.symbol lexer "=>")
arrow_   = void (Token.symbol lexer "->")
doarrow_ = void (Token.symbol lexer "<-")
equal_   = void (Token.symbol lexer "=")
wild_    = void (Token.symbol lexer "_")
plus_    = void (Token.symbol lexer "+")
-- minus_   = Token.reserved lexer "-"

colon_   = void (Token.colon lexer)
dot_     = void (Token.dot lexer)
comma_   = void (Token.comma lexer)
semi_    = void (Token.semi lexer)

lbrack_  = Token.reserved lexer "["
rbrack_  = Token.reserved lexer "]"
val_     = Token.reserved lexer "val"
fun_     = Token.reserved lexer "fun"
sig_     = Token.reserved lexer "sig"
type_    = Token.reserved lexer "type"
infix_   = Token.reserved lexer "infix"
infixl_  = Token.reserved lexer "infixl"
infixr_  = Token.reserved lexer "infixr"
fix_     = Token.reserved lexer "fix"
let_     = Token.reserved lexer "let"
in_      = Token.reserved lexer "in"
case_    = Token.reserved lexer "case"
of_      = Token.reserved lexer "of"
end_     = Token.reserved lexer "end"
if_      = Token.reserved lexer "if"
then_    = Token.reserved lexer "then"
else_    = Token.reserved lexer "else"
true_    = Token.reserved lexer "true"
false_   = Token.reserved lexer "false"
left_    = Token.reserved lexer "left"
right_   = Token.reserved lexer "right"
fn_      = Token.reserved lexer "fn"
do_      = Token.reserved lexer "do"
io_      = Token.reserved lexer "IO"
