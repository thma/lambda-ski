module Parser 
(
    Environment
  , Expr(..)
  , parseEnvironment
)
where

import Data.List ( foldl1' )
import Data.Maybe ( catMaybes, fromMaybe )
import Text.Parsec
import Data.Functor.Identity (Identity)
import           System.IO            (hSetEncoding, stdin, stdout, utf8)

type Parser = Parsec String ()

infixl 5 :@
data Expr =
    Expr :@ Expr
  | Var String
  | Int Integer
  | Lam String Expr
  deriving (Eq, Show)

type Environment = [(String, Expr)]

num :: Parser Expr
num = do
  sign   <- many (oneOf "-")
  digits <- many1 digit <* ws
  case length sign of
    0 -> return $ Int (read digits)
    _ -> return $ Int (read $ "-" ++ digits)

source :: Parser Environment
source = catMaybes <$> many maybeLet where
  maybeLet :: ParsecT String () Identity (Maybe (String, Expr))
  maybeLet = between ws newline $ optionMaybe $ (,) <$> var <*> (str "=" >> term)
  term :: ParsecT String () Identity Expr
  term = try num
     <|> lam
     <|> app
  lam :: ParsecT String () Identity Expr
  lam = flip (foldr Lam) <$> between lam0 lam1 (many1 var) <*> term where
    lam0 = str "\\" <|> str "\0955"
    lam1 = str "->" <|> str "."
  app :: ParsecT String () Identity Expr
  app = foldl1' (:@) <$> many1
    (
     try num
     <|> Var <$> var
     <|> between (str "(") (str ")") term )

  var :: ParsecT String u Identity String
  var   = (mathOp <|> many1 alphaNum) <* ws

  mathOp :: ParsecT String u Identity String
  mathOp = string "+" <|> string "/" <|> string "*" -- <|> string "-"

  str = (>> ws) . string

ws :: ParsecT String u Identity ()
ws = many (oneOf " \t") >> optional (try $ string "--" >> many (noneOf "\n"))


parseEnvironment :: String -> Either ParseError Environment
parseEnvironment s = parse source "" (s ++ "\n")


