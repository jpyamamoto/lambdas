module SystemF.Parser (parse) where

import Control.Monad.State
import Data.Void
import Data.List (elemIndex)

import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
import Text.Megaparsec hiding (parse, State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import SystemF.Syntax
import Components (ProgramParser)
import Error

type Parser = ParsecT Void String (State ( [String] -- Bound Variables
                                         , [String] -- Types
                                         , [String] -- Temp Definitions
                                         ))

---- AUX ----
-- Triplet projections
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

-- Trifunctor mappings
first :: (a -> a) -> ((a, b, c) -> (a, b, c))
first f (a, b, c) = (f a, b, c)

second :: (b -> b) -> ((a, b, c) -> (a, b, c))
second f (a, b, c) = (a, f b, c)

third :: (c -> c) -> ((a, b, c) -> (a, b, c))
third f (a, b, c) = (a, b, f c)
-------------

reservedWords :: [String]
reservedWords = ["lambda", "forall"]

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: String -> Parser String
reserved s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

typeArg :: Parser Term
typeArg = brackets (ArgT <$> typeVal)

forall :: Parser String
forall = reserved "forall" <|> symbol "∀"

lambda :: Parser String
lambda = reserved "lambda" <|> symbol "λ" <|> symbol "\\"

upperLambda :: Parser String
upperLambda = reserved "Lambda" <|> symbol "Λ"

dot :: Parser String
dot = symbol "."

isdefined :: Parser String
isdefined = symbol "=" <|> symbol "≐"

istypedefined :: Parser String
istypedefined = symbol "::"

identifierWord :: Parser String
identifierWord = (lexeme . try) (p >>= check)
  where p = (:) <$> lowerChar <*> many alphaNumChar
        check w = if w `elem` reservedWords
                     then getSourcePos
                          >>= (\l -> fail $
                                "[Line " ++ show l ++"] keyword " ++ show w ++
                                " cannot be used as variable")
                     else return w

typeWord :: Parser String
typeWord = (lexeme . try) p
  where p = (:) <$> upperChar <*> many alphaNumChar

forallAbs :: Parser Type
forallAbs = do
  forall
  v <- typeWord
  modify (second (v:))
  dot
  t <- typeVal
  modify (second tail)
  return $ All v t

typeVal :: Parser Type
typeVal = makeExprParser basic typeTable
  where basic =
              parens typeVal
          <|> typeVariable
          <|> try forallAbs

typeAnnotation :: Parser Type
typeAnnotation = symbol ":" *> typeVal

-- Type Arrows
typeTable :: [[Operator Parser Type]]
typeTable = [ [ InfixR (Arrow <$ symbol "->") ] ]

variable :: Parser Term
variable = do
  v <- identifierWord
  list <- get

  let index = elemIndex v (fst3 list)

  case index of
    Nothing -> if v `elem` thd3 list
      then return $ Var index v
      else getSourcePos >>= (\l -> fail $ "[Line " ++ show l ++ "] " ++ show v ++
                              " appears as a free variable")
    _ -> return $ Var index v

typeVariable :: Parser Type
typeVariable = do
  v <- typeWord
  list <- get

  let index = elemIndex v (snd3 list)

  case index of
    Nothing -> if v `elem` thd3 list
      then return $ VarT index v
      else getSourcePos >>= (\l -> fail $ "[Line " ++ show l ++ "] " ++ show v ++
                              " appears as a free type variable")
    _ -> return $ VarT index v

typeAbs :: Parser Term
typeAbs = do
  upperLambda
  v <- typeWord
  modify (second (v:))
  dot
  t <- term
  modify (second tail)
  return $ AbsT v t

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
term = foldl1 App <$> some (
      parens term
  <|> typeArg
  <|> try lamAbs
  <|> try typeAbs
  <|> try nondef
      )
  where nondef = variable <* notFollowedBy isdefined

definition :: Parser Instruction
definition = termDefinition <|> typeDefinition

termDefinition :: Parser Instruction
termDefinition = do
  n <- identifierWord
  isdefined

  modify (third (n:))

  DefTerm n <$> term

typeDefinition :: Parser Instruction
typeDefinition = do
  n <- typeWord
  istypedefined
  ty <- typeVal

  modify (third (n:))

  return $ DefType n ty

commandEval :: Parser Instruction
commandEval = do
  reserved ":e"
  line <- getSourcePos
  Eval (sourceLine line) <$> term

commandInfo :: Parser Instruction
commandInfo = do
  reserved ":i"
  line <- getSourcePos
  Info (sourceLine line) <$> term

commandType :: Parser Instruction
commandType = do
  reserved ":t"
  line <- getSourcePos
  Type (sourceLine line) <$> term

program :: Parser [Instruction]
program = sc *> some (definition <|> commandEval <|> commandInfo <|> commandType) <* eof

parse :: ProgramParser Instruction
parse text = case runState (runParserT program "" text) ([], [], []) of
  (Left err, _)  -> Left . ParseError $ errorBundlePretty err
  (Right ast, _) -> Right ast
