{-# LANGUAGE InstanceSigs #-}
module Parser
  ( Environment,
    Expr (..),
    parseEnvironmentEither,
    parseEnvironment
  )
where

import           Data.Functor.Identity (Identity)
import           Data.List             (foldl1')
import           Data.Maybe            (catMaybes)
import           Text.Parsec

type Parser = Parsec String ()

data Expr
  = App Expr Expr
  | Var String
  | Int Integer
  | Lam String Expr
  deriving (Eq, Show)

-- instance Show Expr where
--   show :: Expr -> String
--   show (Lam s t)  = "\955" ++ s ++ showB t where
--     showB (Lam x y) = " " ++ x ++ showB y
--     showB expr      = "->" ++ show expr
--   show (Var s)    = s
--   show (Int i)    = show i
--   show (App x y)  = showL x ++ showR y where
--     showL (Lam _ _) = "(" ++ show x ++ ")"
--     showL _         = show x
--     showR (Var s)   = ' ':s
--     showR _         = "(" ++ show y ++ ")"

type Environment = [(String, Expr)]


num :: Parser Expr
num = do
  sign <- many (oneOf "-")
  digits <- many1 digit <* ws
  case length sign of
    0 -> return $ Int (read digits)
    _ -> return $ Int (read $ "-" ++ digits)

source :: Parser Environment
source = catMaybes <$> many maybeLet
  where
    maybeLet :: ParsecT String () Identity (Maybe (String, Expr))
    maybeLet = between ws newline $ optionMaybe $ (,) <$> var <*> (str "=" >> term)
    term :: ParsecT String () Identity Expr
    term =
      try num
        <|> lam
        <|> app
    lam :: ParsecT String () Identity Expr
    lam = flip (foldr Lam) <$> between lam0 lam1 (many1 var) <*> term
      where
        lam0 = str "\\" <|> str "\0955"
        lam1 = str "->" <|> str "."
    app :: ParsecT String () Identity Expr
    app =
      foldl1' App 
        <$> many1
          ( try num
              <|> Var <$> var
              <|> between (str "(") (str ")") term
          )

    var :: ParsecT String u Identity String
    var = (mathOp <|> many1 alphaNum) <* ws

    mathOp :: ParsecT String u Identity String
    mathOp = string "+" <|> string "/" <|> string "*" -- <|> string "-"
    str = (>> ws) . string

ws :: ParsecT String u Identity ()
ws = many (oneOf " \t") >> optional (try $ string "--" >> many (noneOf "\n"))

parseEnvironmentEither :: String -> Either ParseError Environment
parseEnvironmentEither s = parse source "" (s ++ "\n")

parseEnvironment :: String -> Environment
parseEnvironment source =
  case parseEnvironmentEither source of
    Left err  -> error $ show err
    Right env -> env
