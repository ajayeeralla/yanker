module TypeDatabase where

import TypeHierarchy

import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char


-- Type database parser
lexerDB = P.makeTokenParser (emptyDef { reservedOpNames = ["/","\\","[","]","\n","\t"," "] })

parserLineDB :: Parser (Int,LambekFun)
parserLineDB = do
  occurs <- P.natural lexerDB
  char ','
  lambekType <- parserLF
  -- char '\n'
  return (fromIntegral occurs,lambekType)

parserDB = do
  ldb <- many parserLineDB
  eof
  return ldb


