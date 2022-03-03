module Pratt where

import Control.Monad (void)
import qualified Data.Char as C
import qualified Data.List as L
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
  | PostDecrement Expr
  | Custom String [Expr]
  deriving (Eq, Show)

identifier :: Parser String
identifier = (:) <$> C.letterChar <*> many alphaNumChar

keyword :: String -> Parser ()
keyword kw = string kw *> notFollowedBy alphaNumChar

data Part
  = Hole Int
  | Chunk String
  deriving (Show, Eq)

customOperators :: [(String, [Part])]
customOperators =
  [ -- Prefix
    ("group", [Chunk "(", Hole 0, Chunk ")"])
  , ("tuple", [Chunk "(", Hole 0, Chunk ",", Hole 0, Chunk ")"])
  , ("increment", [Chunk "++", Hole 2000])
  , ("cond", [Chunk "if", Hole 0, Chunk "then", Hole 0, Chunk "else", Hole 0])
  , -- Non-prefix
    ("ternary-cond", [Hole 50, Chunk "?", Hole 0, Chunk ":", Hole 0])
  , ("plus", [Hole 100, Chunk "+", Hole 100])
  , ("times", [Hole 200, Chunk "*", Hole 200])
  , ("exp", [Hole 300, Chunk "^", Hole (300 - 1)])
  , ("post-decrement", [Hole 2000, Chunk "--"])
  ]

isChunk :: Part -> Bool
isChunk (Chunk _) = True
isChunk (Hole _) = False

chunkToParser :: String -> Parser ()
chunkToParser s | C.isAlpha (last s) = keyword s
chunkToParser s = void $ string s

type PrefixOperatorDef = (Int -> Parser Expr) -> Parser Expr
type InfixOperatorDef = (Int, Parser (), Expr -> (Int -> Parser Expr) -> Parser Expr)
type OperatorsDef = ([PrefixOperatorDef], [InfixOperatorDef])

genParser :: (Int -> Parser Expr) -> [Part] -> Parser [Expr]
genParser _ [] = pure []
genParser pExpr (Hole prec : rest) =
  (:) <$> (space *> pExpr prec) <*> genParser pExpr rest
genParser pExpr (Chunk s : rest) =
  (space *> chunkToParser s) *> genParser pExpr rest

customToDef :: [(String, [Part])] -> OperatorsDef
customToDef [] = ([], [])
customToDef ((name, parts@(Chunk _ : _)) : rest) =
  let (prefixOps, infixOps) = customToDef rest
      parser = \pExpr -> Custom name <$> genParser pExpr parts
   in (parser : prefixOps, infixOps)
customToDef ((name, Hole prec : Chunk pref : parts) : rest) =
  let (prefixOps, infixOps) = customToDef rest
      parser =
        ( prec
        , space *> void (chunkToParser pref)
        , \left pExpr -> Custom name <$> ((left :) <$> genParser pExpr parts)
        )
   in (prefixOps, parser : infixOps)

compileOperators :: [(String, [Part])] -> Int -> OperatorsDef
compileOperators ops appPrec =
  let follow = [s | (_, parts) <- ops, Chunk s <- drop 1 . filter isChunk $ parts]
      avoid = map parseFollow . L.nub $ follow
      parseFollow s | C.isAlpha (last s) = keyword s
      parseFollow s = void $ string s
      app =
        ( appPrec
        , space1 *> notFollowedBy (choice (avoid ++ [eof]))
        , \left pExpr -> App left <$> pExpr appPrec
        )
      (prefixOps, infixOps) = customToDef ops
   in ( prefixOps
          ++ [ const (Num <$> L.decimal)
             , const (Name <$> identifier)
             ]
      , infixOps ++ [app]
      )

pExpression :: OperatorsDef -> Int -> Parser Expr
pExpression ops@(prefixOps, _) rbp = do
  left <- choice $ map (try . ($ pExpression ops)) prefixOps
  pLoop ops rbp left

pLoop :: OperatorsDef -> Int -> Expr -> Parser Expr
pLoop ops@(_, infixOps) rbp left = do
  alt <-
    optional . lookAhead . choice
      . map (\t@(_, prefix, _) -> t <$ try prefix)
      $ infixOps

  case alt of
    Just (lbp, consume, parse) -> do
      if rbp < lbp
        then do
          _ <- consume
          pLoop ops rbp =<< parse left (pExpression ops)
        else pure left
    Nothing -> pure left

traverseCustom :: (String -> [Expr] -> Expr) -> Expr -> Expr
traverseCustom t e =
  case e of
    (Custom name exprs) ->
      let realExprs = map (traverseCustom t) exprs
       in t name realExprs
    (App l r) -> App (traverseCustom t l) (traverseCustom t r)
    t@(Name _) -> t
    t@(Num _) -> t

unCustom :: String -> [Expr] -> Expr
unCustom name exprs =
  let op =
        case name of
          "group" -> head
          "tuple" -> binary Tuple
          "increment" -> unary Increment
          "cond" -> ternary Cond
          "ternary-cond" -> ternary Cond
          "plus" -> binary Plus
          "times" -> binary Times
          "exp" -> binary Exponent
          "post-decrement" -> unary PostDecrement
      unary f [e1] = f e1
      binary f [e1, e2] = f e1 e2
      ternary f [e1, e2, e3] = f e1 e2 e3
   in op exprs

pExpr :: Parser Expr
pExpr =
  traverseCustom unCustom
    <$> pExpression (compileOperators customOperators 1000) 0
