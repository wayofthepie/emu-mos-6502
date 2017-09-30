{-# LANGUAGE OverloadedStrings #-}
module Assembler where

import Control.Monad (void)
import GHC.Word
import qualified Data.Text as T -- from the "text" package
import Text.Megaparsec hiding (Label, label)
import qualified Text.Megaparsec.Lexer as L
import Numeric (readHex)


type Parser = Parsec Dec T.Text

newtype Label = Label T.Text deriving Show

newtype IsImmediate = IsImmediate Bool deriving Show

data Operand = Operand IsImmediate T.Text deriving Show

newtype Mnemonic = Mnemonic T.Text deriving Show

newtype Var = Var Label  deriving Show

newtype Val = Val Operand deriving Show

data LabelOrOperand = Lbl Label | Op Operand deriving Show

data Expr
  = Instruction (Maybe Label) Mnemonic (Maybe LabelOrOperand)
  | Assignment Var Val
  deriving Show

-- | Our top level parser, looking at the grammar this is simply either an instruction or an
-- assigment.
parseExpression :: Parser Expr
parseExpression = try assignment <|> instruction

-- | Parse an assignment of a label (a variable name in this case, of type 'Var') to a byte
-- value, with immediate or non-immediate addressing.
assignment :: Parser Expr
assignment = do
  labelVal <- label
  parseEquals
  operandVal <- operand
  pure $ Assignment (Var labelVal) (Val operandVal)
 where
  parseEquals = spaceEater *> char '=' *> spaceEater

-- | Parse an instruction.
instruction :: Parser Expr
instruction = lexeme $ Instruction
  <$> (optional $ try labelAssign)
  <*> mnemonic
  <*> optional (Op <$> operand)

-- | Parse an operand, e.g. #$2020.
operand :: Parser Operand
operand = lexeme $ Operand
  <$> (option (IsImmediate False) (char '#' >> (pure $ IsImmediate True)))
  <*> bytes

-- | Parse one or two bytes.
bytes :: Parser T.Text
bytes = lexeme $ do
  char '$'
  byte' <- byte
  anotherByte <- option "" byte
  pure $ T.append byte' anotherByte

-- | Parse the assignment of a label to a location in the program.
labelAssign :: Parser Label
labelAssign = lexeme $  label <* char ':'

-- | Parse a label, recognizes all alpha numeric characters.
label :: Parser Label
label = lexeme $ Label . T.pack <$> ((:) <$> letterChar <*>  many alphaNumChar)

-- | Parse a mnemonic string, recognizes all 3 letter strings.
mnemonic :: Parser Mnemonic
mnemonic = lexeme $ Mnemonic . T.pack <$> op
 where
  op = count 3 letterChar

-- | Parse a two letter hex string.
byte :: Parser T.Text
byte = do
  high <- hexDigitChar
  low <- hexDigitChar
  pure $ T.pack [high,low]

-- | Eats space and comments! Yum!
spaceEater :: Parser ()
spaceEater = L.space
  (void spaceChar)
  (L.skipLineComment ";")
  (L.skipBlockComment "/*" "*/")

-- | A single unit, removes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceEater
