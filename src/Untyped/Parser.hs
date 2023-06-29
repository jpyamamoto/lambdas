module Untyped.Parser (parse) where

import Control.Monad.State
import Text.Megaparsec hiding (parse, State, ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void
import Data.List (elemIndex)

import Untyped.Definitions
import Components (ProgramParser, Error (..))

type Parser = ParsecT Void Text (State [String])

reservedWords :: [String]
reservedWords = ["lambda"]

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

dot :: Parser Text
dot = symbol "."

isdefined :: Parser Text
isdefined = symbol "=" <|> symbol "≐"

identifierWord :: Parser String
identifierWord = (lexeme . try) (p >>= check)
  where p = (:) <$> lowerChar <*> many alphaNumChar
        check w = if w `elem` reservedWords
                     then getSourcePos
                          >>= (\l -> fail $
                                "[Line " ++ show l ++"] keyword " ++ show w ++
                                " cannot be used as variable")
                     else return w

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
term = foldl1 App <$> some (parens term <|> try lamAbs <|> try nondef)
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
  (Left err, _)  -> Left . ParseError $ errorBundlePretty err
  (Right ast, _) -> Right ast
