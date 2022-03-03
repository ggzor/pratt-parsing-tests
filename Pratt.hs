module Pratt where

import Control.Monad (void)
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe)
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
  | Unwrap Expr
  | Custom String [Expr]
  deriving (Eq, Show)

identifier :: Parser String
identifier = (:) <$> C.letterChar <*> many alphaNumChar

keyword :: String -> Parser ()
keyword kw = string kw *> notFollowedBy alphaNumChar

data SpaceSpec = Optional | Require | Forbid
  deriving (Show, Eq)

data Part
  = Hole SpaceSpec Int
  | Chunk SpaceSpec String
  deriving (Show, Eq)

toSpaceParser :: SpaceSpec -> Parser ()
toSpaceParser Optional = space
toSpaceParser Require = space1
toSpaceParser Forbid = pure ()

isChunk :: Part -> Bool
isChunk (Chunk _ _) = True
isChunk (Hole _ _) = False

chunkToParser :: [String] -> String -> Parser ()
chunkToParser chunks s =
  let avoid = map (chunkToParser []) . mapMaybe (L.stripPrefix s) . filter (/= s) $ chunks
   in if C.isAlpha (last s)
        then keyword s <* notFollowedBy (choice avoid)
        else void $ string s <* notFollowedBy (choice avoid)

type PrefixOperatorDef = (Int -> Parser Expr) -> Parser Expr
type InfixOperatorDef = (Int, Parser (), Expr -> (Int -> Parser Expr) -> Parser Expr)
type OperatorsDef = ([PrefixOperatorDef], [InfixOperatorDef])

genParser :: [String] -> (Int -> Parser Expr) -> [Part] -> Parser [Expr]
genParser chunks _ [] = pure []
genParser chunks pExpr (Hole spaceSpec prec : rest) =
  (:) <$> (toSpaceParser spaceSpec *> pExpr prec) <*> genParser chunks pExpr rest
genParser chunks pExpr (Chunk spaceSpec s : rest) =
  (toSpaceParser spaceSpec *> chunkToParser chunks s) *> genParser chunks pExpr rest

customToDef :: [String] -> [(String, [Part])] -> OperatorsDef
customToDef chunks [] = ([], [])
customToDef chunks ((name, parts@(Chunk _ _ : _)) : rest) =
  let (prefixOps, infixOps) = customToDef chunks rest
      parser = \pExpr -> Custom name <$> genParser chunks pExpr parts
   in (parser : prefixOps, infixOps)
customToDef chunks ((name, Hole _ prec : Chunk spaceSpec pref : parts) : rest) =
  let (prefixOps, infixOps) = customToDef chunks rest
      parser =
        ( prec
        , toSpaceParser spaceSpec *> void (chunkToParser chunks pref)
        , \left pExpr -> Custom name <$> ((left :) <$> genParser chunks pExpr parts)
        )
   in (prefixOps, parser : infixOps)

compileOperators :: [(String, [Part])] -> Int -> OperatorsDef
compileOperators ops appPrec =
  let follow = [s | (_, parts) <- ops, Chunk _ s <- drop 1 . filter isChunk $ parts]
      allChunks = [s | (_, parts) <- ops, Chunk _ s <- filter isChunk parts]
      avoid = map parseFollow . L.nub $ follow
      parseFollow s | C.isAlpha (last s) = keyword s
      parseFollow s = void $ string s
      app =
        ( appPrec
        , space1 *> notFollowedBy (choice (avoid ++ [eof]))
        , \left pExpr -> App left <$> pExpr appPrec
        )
      (prefixOps, infixOps) = customToDef allChunks ops
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

fChunk = Chunk Forbid
fHole = Hole Forbid

oChunk = Chunk Optional
oHole = Hole Optional

rChunk = Chunk Require
rHole = Hole Require

customOperators :: [(String, [Part])]
customOperators =
  [ -- Prefix
    ("group", [oChunk "(", oHole 0, oChunk ")"])
  , ("tuple", [oChunk "(", oHole 0, oChunk ",", oHole 0, oChunk ")"])
  , ("increment", [oChunk "++", fHole 2000])
  , ("cond", [oChunk "if", oHole 0, oChunk "then", oHole 0, oChunk "else", oHole 0])
  , -- Non-prefix
    ("unwrap", [fHole 2000, fChunk "?"])
  , ("ternary-cond", [fHole 50, rChunk "?", oHole 0, oChunk ":", oHole 0])
  , ("plus", [fHole 100, oChunk "+", oHole 100])
  , ("times", [fHole 200, oChunk "*", oHole 200])
  , ("exp", [fHole 300, oChunk "^", oHole (300 - 1)])
  , ("post-decrement", [fHole 2000, fChunk "--"])
  ]

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
          "unwrap" -> unary Unwrap
      unary f [e1] = f e1
      binary f [e1, e2] = f e1 e2
      ternary f [e1, e2, e3] = f e1 e2 e3
   in op exprs

pExpr :: Parser Expr
pExpr =
  traverseCustom unCustom
    <$> pExpression (compileOperators customOperators 1000) 0
