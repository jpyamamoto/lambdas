module Typed.Parser (parse) where

import Control.Monad.State
import Text.Megaparsec hiding (parse, State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void
import Data.List (elemIndex)
import Data.Functor (($>))

import Typed.Definitions
import Components (ProgramParser)

type Parser = ParsecT Void Text (State [String])

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser Text
reserved s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

parens :: Parser Term -> Parser Term
parens = between (symbol "(") (symbol ")")

lambda :: Parser Text
lambda = reserved "lambda" <|> symbol "λ" <|> symbol "\\"

arrow :: Parser Text
arrow = symbol "->"

dot :: Parser Text
dot = symbol "."

colon :: Parser Text
colon = symbol ":"

typeIdentifier :: Parser Type
typeIdentifier = Simple <$> ((:) <$> upperChar <*> many alphaNumChar)

arrowType :: Parser Type
arrowType = Arrow <$> (typeVal <* arrow) <*> typeVal

typeVal :: Parser Type
typeVal = typeIdentifier <|> try arrowType

typedTerm :: Parser Term
typedTerm = do
  t <- term
  colon
  Type t <$> typeVal

isdefined :: Parser Text
isdefined = symbol "=" <|> symbol "≐"

identifierWord :: Parser String
identifierWord = (lexeme . try) (p >>= check)
  where p = (:) <$> lowerChar <*> many alphaNumChar
        check w = if w `elem` ["true", "false"]
                     then getSourcePos
                          >>= (\l -> fail $
                                "[Line " ++ show l ++"] keyword " ++ show w ++
                                " cannot be used as variable")
                     else return w

natural :: Parser Term
natural = Nat <$> lexeme L.decimal

boolean :: Parser Term
boolean = Bool <$> (parseTrue <|> parseFalse)
  where parseTrue = reserved "true" $> True
        parseFalse = reserved "false" $> False

variable :: Parser Term
variable = do
  v <- identifierWord
  list <- get

  return $ Var (elemIndex v list) v

lamAbs :: Parser Term
lamAbs = do
  lambda
  v <- identifierWord
  modify (v:)
  dot
  t <- term
  modify tail
  return $ Abs v t

term :: Parser Term
term = foldl1 App <$> some (
      parens term
  <|> boolean
  <|> natural
  <|> try lamAbs
  <|> try nondef)
  where nondef = variable <* notFollowedBy isdefined

definition :: Parser (Instruction Term)
definition = do
  n <- identifierWord
  isdefined
  Defn n <$> term

commandEval :: Parser (Instruction Term)
commandEval = do
  reserved ":e"
  line <- getSourcePos
  Eval (sourceLine line) <$> term

commandInfo :: Parser (Instruction Term)
commandInfo = do
  reserved ":i"
  line <- getSourcePos
  Info (sourceLine line) <$> term

program :: Parser [Instruction Term]
program = sc *> some (definition <|> commandEval <|> commandInfo) <* eof

parse :: ProgramParser (Instruction Term)
parse text = case runState (runParserT program "" text) [] of
  (Left err, _)  -> Left $ errorBundlePretty err
  (Right ast, _) -> Right ast
