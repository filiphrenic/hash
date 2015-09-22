module Parse (parseProgram) where

import Control.Monad
import Control.Applicative
import Data.Char 
import Text.Parsec (parse, ParseError, try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Token (reservedOp)
import qualified Data.Map as M

import Expressions

-- ==================================================================================

err = error "Parsing error"

simpleParse p = parse p err

betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]  
a <:> b = (:) <$> a <*> b

-- parser to consume the spaces after it
token :: Parser a -> Parser a
token = (<* spaces)

symbol :: Char -> Parser Char
symbol = token . char

-- parses a single word
word :: Parser String
word = many1 (alphaNum <|> char '$' <|> char '.')

-- reads in c1<this>c2
parserWrapped :: Char -> Char -> Parser a -> Parser a
parserWrapped c1 c2 = between (symbol c1) (symbol c2)

-- Some helper functions
brackets :: Parser a -> Parser a
brackets = parserWrapped '[' ']'

parserQuot :: Parser a -> Parser a
parserQuot = parserWrapped '"' '"'

inParens :: Parser a -> Parser a
inParens = parserWrapped '(' ')'

-- ===========================================================

-- parses a variable, needs to start with a letter
pVariable :: Parser Expr
pVariable = Expr <$> (token $ letter <:> many alphaNum)

myNone :: String -> Parser String
myNone s = many1 $ noneOf s

-- everything except quotes
anyThing :: Parser String
anyThing = myNone "\""

pStringInQuotes :: Parser String
pStringInQuotes = parserQuot anyThing

pString :: Parser Expr
pString = Expr <$> (token $ pStringInQuotes <|> word)

pCmdArgs :: Parser [Expr]
pCmdArgs = many (pString)

pStream :: String -> Parser Expr
pStream name = Expr <$> string name *> spaces *> pString
pReadS       = pStream "<"
pWriteS      = pStream ">"
pAppendS     = pStream ">>"

pCmd' :: Parser Cmd
pCmd' = do
  n    <- pString
  args <- pCmdArgs
  r    <- optionMaybe pReadS
  a'   <- optionMaybe $ try pAppendS
  w'   <- optionMaybe pWriteS
  end  <- symbol ';'
  let (w,a) = resolve w' a'
  return $ Cmd n args r w a
    where resolve w a = case w of
            Just p  -> (Just p, False)
            Nothing -> case a of
              Just p -> (Just p, True)
              Nothing -> (Nothing, False)

pCmd = (try pAtoC) <|> pCmd'                                      

-- ===================================================================================

pAssign :: Parser Assign
pAssign = Assign <$> pVariable <*> ( symbol '=' *> pString) <* symbol ';'

pAtoC :: Parser Cmd
pAtoC = do
  (Assign var val) <- pAssign
  return $ Cmd (Expr "assign") [var,val] Nothing Nothing False

pComment :: Parser Comment
pComment = Comment <$> (char '#' *> myNone "\n" <* spaces)

string' :: String -> Parser String
string' = token . string

compTable :: [Parser Comp]
compTable = zipWith binary ["==","/=",">=",">", "<=","<"] $
                           [CEQ, CNE, CGE, CGT, CLE, CLT]
  where binary name f = f <$> pString <*> (rest name)
        rest name     = string' name *> pString

pComp :: Parser Comp
pComp = choice $ map try compTable

compOp name f = Infix (f <$ (string' name)) AssocLeft
compAnd = compOp "&&" And
compOr  = compOp "||" Or
compNot = Prefix (Not <$ (symbol '!'))

predTable = [[compNot], [compAnd], [compOr]]

pPred' :: Parser Pred
pPred' = Pred <$> pComp

inParensPred :: Parser Pred
inParensPred = inParens pPred'

pPred :: Parser Pred
pPred = buildExpressionParser predTable parser'
  where parser' = inParensPred <|> pPred'

pCmdBlock :: Parser [Cmd]
pCmdBlock = parserWrapped '{' '}' p <|> p
  where p = many $ spaces *> pCmd

pCond :: Parser Conditional
pCond = do
  string' "if"
  p  <- pPred
  t  <- pCmdBlock
  spaces
  e' <- optionMaybe $ string' "else" >> pCmdBlock
  return $ case e' of
    Nothing -> If     p t
    Just e  -> IfElse p t e

pWhile :: Parser Loop
pWhile = string' "while" >> pPred >>= \p -> pCmdBlock >>= return . While p

pDoWhile :: Parser Loop
pDoWhile = string' "do" >> pCmdBlock >>= f
  where f b = string' "while" >> pPred <* symbol ';' >>= return . flip DoWhile b

pFor :: Parser Loop
pFor = do
  string' "for"
  symbol '('
  i <- pAssign
  spaces
  c <- pPred
  symbol ';'
  spaces
  s <- pAssign
  symbol ')'
  For i c s <$> pCmdBlock

pLoop :: Parser Loop
pLoop = choice [pWhile, pDoWhile, pFor]

pTLExpr :: Parser TLExpr
pTLExpr = choice $ map try [ TLCmt <$> pComment
                           , TLCnd <$> pCond
                           , TLClp <$> pLoop
                           , TLCmd <$> pCmd ]

parser :: Parser [TLExpr]
parser = many1 pTLExpr

parseProgram :: String -> Either ParseError [TLExpr]
parseProgram = betterParse parser


s = concat [ "do {"
           , "  rmdir a;"
           , "  mkdir b;"
           , "} while (a>b);"
           ]

s1 = concat [ "for ( i=1 ; i<2; i=2 ) {\n"
            , "some cmd;"
            , "}" ]

s2 = concat [ "while a==1 && b==2 {"
           , "echo uspjelo;"
           , "a = 2;\n"
           , "}" ]