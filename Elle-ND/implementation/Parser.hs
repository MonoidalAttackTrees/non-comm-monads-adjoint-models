{-# LANGUAGE NoMonomorphismRestriction, PackageImports,
             FlexibleContexts, PackageImports #-}

------------------------------------------------------------------------
-- This file contains the ILL-Impl parsers utilizing Parsec library.  --
------------------------------------------------------------------------

module Parser where

import Prelude
import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Monad
import Data.Functor.Identity

import Syntax

lexer = haskellStyle {
  Token.reservedNames = ["let", "be", "in", "for", "as", "triv", "Unit"],
  Token.reservedOpNames = ["(x)", "tens", "-o", "\\", ":", ":show", ":t", ":type", ":u",
                           ":unfold", ":d", ":dump"]
}

tokenizer  = Token.makeTokenParser lexer
ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
angles     = Token.angles tokenizer
brackets   = Token.brackets tokenizer
braces     = Token.braces tokenizer
ws         = Token.whiteSpace tokenizer
natural    = Token.natural tokenizer
dot        = Token.dot tokenizer
comma      = Token.comma tokenizer
colon      = Token.colon tokenizer
symbol     = Token.symbol tokenizer
identifier = Token.identifier tokenizer

unexpColon msg = unexpected msg

------------------------------------------------------------------------
-- Type parsers                                                       --
------------------------------------------------------------------------
tyUnit_T = do
  reservedOp "Unit"
  return Unit_T

tyUnit_S = do
  reservedOp "Unit"
  return Unit_S

------------------------------------------------------------------------
-- Type parser tables                                                 --
------------------------------------------------------------------------
tyTable_T = [[binOp_T AssocLeft "(x)" (\d r -> Tensor_T d r)],
           [binOp_T AssocRight "-o" (\d r -> Imp_T d r)]]
binOp_T assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op; return f}) assoc
typeParser_T = buildExpressionParser tyTable_T typeParser'_T
typeParser'_T = parens typeParser_T <|> tyUnit_T

tyTable_S = [[binOp_S AssocLeft "(x)" (\d r -> Tensor_S d r)],
           [binOp_S AssocRight "->" (\d r -> ImpL_S d r)],
           [binOp_S AssocRight "<-" (\d r -> ImpR_S d r)]]
binOp_S assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op; return f}) assoc
typeParser_S = buildExpressionParser tyTable_S typeParser'_S
typeParser'_S = parens typeParser_S <|> tyUnit_S

------------------------------------------------------------------------
-- Term_T parsers                                                     --
------------------------------------------------------------------------
aterm_T = parens termParser_T <|> trivParse_T <|> var_T
termParser_T = lamParse_T <|> try letTParse_T <|> letUParse_T <|> tensParse_T <|> appParse_T <?> "Parser error"

var_T = var'_T varName_T Var_T
var'_T p c = do
  var_name_T <- p
  return (c var_name_T)

varName_T = varName'_T isUpper "Term variables must begin with a lowercase letter."
varName'_T p msg = do
  n <- identifier
  when ((length n) > 0) $
    let h = head n in
      when (p h || isNumber h) $ unexpColon (n++" : "++msg)
  return . s2n $ n

trivParse_T = do
  reserved "triv"
  return Triv_T

lamParse_T = do
  reserved "\\"
  symbol "("
  name <- varName_T
  colon
  ty <- typeParser_T
  symbol ")"
  dot
  body <- termParser_T
  return $ Lam_T ty . bind name $ body

appParse_T = do
  l <- many1 aterm_T
  return $ foldl1 App_T l

tensParse_T = do
  reserved "tens"
  t1 <- termParser_T
  symbol ","
  t2 <- termParser_T
  return $ Tens_T t1 t2

letUParse_T = do
  reserved "let"
  reserved "triv"
  reservedOp "="
  t1 <- termParser_T     -- change to varName?
  reserved "in"
  t2 <- termParser_T
  return $ LetU_T t1 t2

letTParse_T = do
  reserved "let"
  x <- varName_T
  reservedOp "(x)"
  y <- varName_T
  colon
  ty1 <- typeParser_T
  reservedOp "(x)"
  ty2 <- typeParser_T
  reservedOp "be"
  t1 <- termParser_T
  reserved "in"
  t2 <- termParser_T
  return $ LetT_T t1 (Tensor_T ty1 ty2) (bind x (bind y t2))

------------------------------------------------------------------------
-- Term_S parsers                                                     --
------------------------------------------------------------------------
aterm_S = parens termParser_S <|> trivParse_S <|> var_S
termParser_S = lamLParse_S <|> lamRParse_S <|> try letTParse_S <|> letUParse_S <|> tensParse_S <|> appLParse_S <|> appRParse_S <|> exParse_S <?> "Parser error"

var_S = var'_S varName_S Var_S
var'_S p c = do
  var_name_S <- p
  return (c var_name_S)

varName_S = varName'_S isUpper "Term variables must begin with a lowercase letter."
varName'_S p msg = do
  n <- identifier
  when ((length n) > 0) $
    let h = head n in
      when (p h || isNumber h) $ unexpColon (n++" : "++msg)
  return . s2n $ n

trivParse_S = do
  reserved "triv"
  return Triv_S

lamLParse_S = do
  reserved "\\l"
  symbol "("
  name <- varName_S
  colon
  ty <- typeParser_S
  symbol ")"
  dot
  body <- termParser_S
  return $ LamL_S ty . bind name $ body

lamRParse_S = do
  reserved "\\r"
  symbol "("
  name <- varName_S
  colon
  ty <- typeParser_S
  symbol ")"
  dot
  body <- termParser_S
  return $ LamR_S ty . bind name $ body

appLParse_S = do
  l <- many1 aterm_S
  return $ foldl1 AppL_S l

appRParse_S = do
  l <- many1 aterm_S
  return $ foldl1 AppR_S l

tensParse_S = do
  reserved "tens"
  s1 <- termParser_S
  symbol ","
  s2 <- termParser_S
  return $ Tens_S s1 s2

letUParse_S = do
  reserved "let"
  reserved "triv"
  reservedOp "="
  s1 <- termParser_S     -- change to varName?
  reserved "in"
  s2 <- termParser_S
  return $ LetU_S s1 s2

letTParse_S = do
  reserved "let"
  x <- varName_S
  reservedOp "(x)"
  y <- varName_S
  colon
  ty1 <- typeParser_S
  reservedOp "(x)"
  ty2 <- typeParser_S
  reservedOp "be"
  s1 <- termParser_S
  reserved "in"
  s2 <- termParser_S
  return $ LetT_S s1 (Tensor_S ty1 ty2) (bind x (bind y s2))

exParse_S = do
  reserved "ex"
  t1 <- termParser_T
  reserved ", "
  t2 <- termParser_T
  reserved "with"
  x <- varName_S
  comma
  y <- varName_S
  reserved "in"
  s <- termParser_S
  return $ Ex_S t1 t2 (bind x (bind y s))

------------------------------------------------------------------------
-- Functions String -> Term_T or String -> Type_T                     --
------------------------------------------------------------------------
parseTerm_T :: String -> Term_T
parseTerm_T str =
  case parse termParser_T "" str of
    Left e  -> error $ show e
    Right r -> r

parseType_T :: String -> Type_T
parseType_T str =
  case parse typeParser_T "" str of
    Left e  -> error $ show e
    Right r -> r

parseTester_T p str =
  case parse p "" str of
    Left e  -> error $ show e
    Right r -> r

------------------------------------------------------------------------
-- Functions String -> Term_S or String -> Type_S                     --
------------------------------------------------------------------------
parseTerm_S :: String -> Term_S
parseTerm_S str =
  case parse termParser_S "" str of
    Left e  -> error $ show e
    Right r -> r

parseType_S :: String -> Type_S
parseType_S str =
  case parse typeParser_S "" str of
    Left e  -> error $ show e
    Right r -> r

parseTester_S p str =
  case parse p "" str of
    Left e  -> error $ show e
    Right r -> r

------------------------------------------------------------------------
-- Context Parser                                                     --
------------------------------------------------------------------------
tmPairCtxParse_T = do
  nm <- varName_T
  ws
  colon
  ws
  ty <- typeParser_T
  ws
  return (nm, ty)

tmCtxParse_T = tmPairCtxParse_T `sepBy` (Token.symbol tokenizer ",")

tmPairCtxParse_S = do
  nm <- varName_T
  ws
  colon
  ws
  ty <- typeParser_T
  ws
  return (nm, ty)

tmCtxParse_S = tmPairCtxParse_S `sepBy` (Token.symbol tokenizer ",")
------------------------------------------------------------------------
-- Parsers for the REPL                                               --
------------------------------------------------------------------------






















