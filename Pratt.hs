{-# LANGUAGE QuasiQuotes #-}

module Pratt where

import Control.Monad (void)
import qualified Data.Char as C
import Data.Functor (($>))
import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe)
import Data.Void
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.RawString.QQ (r)

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

pSpaceSpec :: Parser SpaceSpec
pSpaceSpec =
  choice
    [ string "\" \"" $> Require
    , hspace1 $> Optional
    , pure Forbid
    ]

-- If you want a prefix with Required space,
-- Then, what you really need a postfix
pFirstPart :: Parser Part
pFirstPart = (($ Forbid) <$> pHole) <|> (($ Optional) <$> pChunk)

pPart :: Parser Part
pPart = try $ do
  sp <- pSpaceSpec
  ($ sp) <$> (pHole <|> pChunk)

pHole, pChunk :: Parser (SpaceSpec -> Part)
pHole = string "_:" *> (flip Hole <$> L.decimal)
pChunk = flip Chunk <$> some (satisfy (not . (\c -> C.isSpace c || c == '_')))

pSyntaxRule :: Parser (String, [Part])
pSyntaxRule =
  (,)
    <$> (char '@' *> many (satisfy (not . C.isSpace)))
    <*> (hspace1 *> ((:) <$> pFirstPart <*> some pPart))

pAllSyntaxRules :: Parser [(String, [Part])]
pAllSyntaxRules = do
  space
  rules <- pSyntaxRule `sepEndBy` space1
  space
  pure rules

customOperatorsStr :: String
customOperatorsStr =
  [r|
@group          ( _:0 )
@tuple          ( _:0 , _:0 )
@increment      ++_:2000
@cond           if _:0 then _:0 else _:0

@unwrap         _:2000?
@ternary-cond   _:50" "? _:0 : _:0
@plus           _:100 + _:100
@times          _:200 * _:200
@exp            _:300 ^ _:299
@post-decrement _:2000--
@index          _:3000[ _:0 ]
|]

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
          other -> Custom other
      unary f [e1] = f e1
      binary f [e1, e2] = f e1 e2
      ternary f [e1, e2, e3] = f e1 e2 e3
   in op exprs

pExpr :: Parser Expr
pExpr = do
  let (Right newOperators) = parse pAllSyntaxRules "<custom>" customOperatorsStr
  traverseCustom unCustom <$> pExpression (compileOperators newOperators 1000) 0
