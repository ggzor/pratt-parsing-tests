module Pratt where

import Control.Monad (void)
import Data.Void
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void String a

data Expr
  = Num Int
  | Name String
  | Tuple Expr Expr
  | Plus Expr Expr
  | Times Expr Expr
  | App Expr Expr
  | Cond Expr Expr Expr
  | Exponent Expr Expr
  | Increment Expr
  | Unwrap Expr
  deriving (Eq, Show)

identifier :: Parser String
identifier = (:) <$> C.letterChar <*> many alphaNumChar

keyword :: String -> Parser ()
keyword kw = string kw *> notFollowedBy alphaNumChar

prefixOperators :: [(Int -> Parser Expr) -> Parser Expr]
prefixOperators =
  [ \pExpr ->
      (space *> char '(' *> space)
        *> pExpr 0
        <* (space *> char ')')
  , \pExpr ->
      (space *> char '(' *> space)
        *> ( Tuple <$> pExpr 0
              <*> ( (space *> char ',')
                      *> (space *> pExpr 0)
                  )
           )
        <* (space *> char ')')
  , \pExpr -> (space *> string "++") *> (Increment <$> pExpr 2000)
  , \pExpr ->
      Cond <$> (space *> keyword "if" *> space *> pExpr 0)
        <*> (space *> keyword "then" *> space *> pExpr 0)
        <*> (space *> keyword "else" *> space *> pExpr 0)
  , const (Num <$> L.decimal)
  , const (Name <$> identifier)
  ]

infixOperators :: [(Int, Parser (), Expr -> (Int -> Parser Expr) -> Parser Expr)]
infixOperators =
  [
    ( 50
    , space *> void (char '?')
    , \left pExpr ->
        Cond left
          <$> (space *> pExpr 0)
          <*> (space *> char ':' *> space *> pExpr 0)
    )
  ,
    ( 100
    , space *> void (char '+')
    , \left pExpr -> Plus left <$> (space *> pExpr 100)
    )
  ,
    ( 200
    , space *> void (char '*')
    , \left pExpr -> Times left <$> (space *> pExpr 200)
    )
  ,
    ( 300
    , space *> void (char '^')
    , \left pExpr -> Exponent left <$> (space *> pExpr (300 - 1))
    )
  ,
    ( 1000
    , space1
        <* notFollowedBy
          ( choice
              [ void $ char ':'
              , void $ char ')'
              , void $ char ','
              , keyword "then"
              , keyword "else"
              , eof
              ]
          )
    , \left pExpr -> App left <$> pExpr 1000
    )
  ,
    ( 1100
    , void (char '?')
    , \left _ -> pure $ Unwrap left
    )
  ]

pExpression :: Int -> Parser Expr
pExpression rbp = do
  left <- choice $ map (try . ($ pExpression)) prefixOperators
  pLoop' rbp left

pLoop' :: Int -> Expr -> Parser Expr
pLoop' rbp left = do
  alt <- optional . choice $
    flip map (filter (\(lbp, _, _) -> rbp < lbp) infixOperators) $
      \(lbp, consume, parse) -> try $ do
        consume
        parse left pExpression
  maybe (pure left) (pLoop' rbp) alt

pLoop :: Int -> Expr -> Parser Expr
pLoop rbp left = do
  alt <-
    optional . lookAhead . choice
      . map (\t@(_, prefix, _) -> t <$ try prefix)
      $ infixOperators

  case alt of
    Just (lbp, consume, parse) -> do
      if rbp < lbp
        then do
          _ <- consume
          pLoop rbp =<< parse left pExpression
        else pure left
    Nothing -> pure left
