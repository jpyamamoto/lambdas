module Typed.Parser (parse) where

import Control.Monad.State
import Data.Text (Text)
import Data.Void
import Data.List (elemIndex)
import Data.Functor (($>))
import Data.Bifunctor (Bifunctor(first, second))

import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
import Text.Megaparsec hiding (parse, State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Typed.Syntax
import Components (ProgramParser)
import Error

type Parser = ParsecT Void Text (State ( [String] -- Bound Variables
                                       , [String] -- Definitions
                                       ))

reservedWords :: [String]
reservedWords = ["true", "false", "if", "iszero", "lambda", "then", "else", "succ", "fix"]

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

parens :: Parser a -> Parser a
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

natural :: Parser Term
natural = Nat <$> lexeme L.decimal

boolean :: Parser Term
boolean = Bool <$> (parseTrue <|> parseFalse)
  where parseTrue = reserved "true" $> True
        parseFalse = reserved "false" $> False

natType :: Parser Text
natType = symbol "ℕ"<|> reserved "Nat" <|> reserved "Natural"

boolType :: Parser Text
boolType = symbol "𝔹"<|> reserved "Bool" <|> reserved "Boolean"

typeVal :: Parser Type
typeVal = makeExprParser basic typeTable
  where basic = (natType $> Natural) <|> (boolType $> Boolean) <|> parens typeVal

typeAnnotation :: Parser Type
typeAnnotation = symbol ":" *> typeVal

-- Type Arrows
typeTable :: [[Operator Parser Type]]
typeTable = [ [ InfixR (Arrow <$ symbol "->") ] ]

ifTerm :: Parser Term
ifTerm = do
  reserved "if"
  g <- term
  reserved "then"
  t <- term
  reserved "else"
  e <- term
  return $ If g t e

iszero :: Parser Term
iszero = reserved "iszero" *> (IsZero <$> term)

succTerm :: Parser Term
succTerm = reserved "succ" *> (Suc <$> term)

fixTerm :: Parser Term
fixTerm = reserved "fix" *> (Fix <$> term)

variable :: Parser Term
variable = do
  v <- identifierWord
  list <- get

  let index = elemIndex v (fst list)

  case index of
    Nothing -> if v `elem` snd list
      then return $ Var index v
      else getSourcePos >>= (\l -> fail $ "[Line " ++ show l ++ "] " ++ show v ++
                              " appears as a free variable")
    _ -> return $ Var index v

lamAbs :: Parser Term
lamAbs = do
  lambda
  v <- identifierWord
  modify (first (v:))
  ty <- typeAnnotation
  dot
  t <- term
  modify (first tail)
  return $ Abs v ty t

term :: Parser Term
term = makeExprParser nonArithTerm arithTable

-- Arithmetic Expressions
arithTable :: [[Operator Parser Term]]
arithTable = [ [ binary "*" Mul ]
             , [ binary "+" Add, binary "-" Min ] ]

binary :: Text -> (t -> t -> t) -> Operator Parser t
binary name f = InfixL (f <$ symbol name)

nonArithTerm :: Parser Term
nonArithTerm = foldl1 App <$> some (
      parens term
  <|> boolean
  <|> natural
  <|> iszero
  <|> succTerm
  <|> ifTerm
  <|> fixTerm
  <|> try lamAbs
  <|> try nondef)
  where nondef = variable <* notFollowedBy isdefined

definition :: Parser (Instruction Term)
definition = do
  n <- identifierWord
  isdefined

  modify (second (n:))

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

commandType :: Parser (Instruction Term)
commandType = do
  reserved ":t"
  line <- getSourcePos
  Type (sourceLine line) <$> term

program :: Parser [Instruction Term]
program = sc *> some (definition <|> commandEval <|> commandInfo <|> commandType) <* eof

parse :: ProgramParser (Instruction Term)
parse text = case runState (runParserT program "" text) ([], []) of
  (Left err, _)  -> Left . ParseError $ errorBundlePretty err
  (Right ast, _) -> Right ast
