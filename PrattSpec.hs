module Main where

import Control.Monad (forM_)
import Pratt
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

tests :: [(String, String, Expr)]
tests =
  [ ("parses numbers", "20", Num 20)
  , ("parses names", "abc", Name "abc")
  , ("parses parenthesized expressions", "(abc)", Name "abc")
  ,
    ( "parses parenthesized expressions with trailing and leading spaces"
    , " ( abc ) "
    , Name "abc"
    )
  ,
    ( "parses plus"
    , "20 + 30"
    , Plus (Num 20) (Num 30)
    )
  ,
    ( "parses plus without spaces"
    , "20+30"
    , Plus (Num 20) (Num 30)
    )
  ,
    ( "parses plus and times with the right precedence"
    , "1 + 2 * 3"
    , Plus (Num 1) (Times (Num 2) (Num 3))
    )
  ,
    ( "associates correctly with parens"
    , "(1 + 2) * 3"
    , Times (Plus (Num 1) (Num 2)) (Num 3)
    )
  ,
    ( "handles right associativity"
    , "2 ^ 3 ^ 4"
    , Exponent (Num 2) (Exponent (Num 3) (Num 4))
    )
  ,
    ( "handles prefix operators"
    , "++3 + 4"
    , Plus (Increment (Num 3)) (Num 4)
    )
  ,
    ( "handles applications"
    , "a 5 + 4"
    , Plus (App (Name "a") (Num 5)) (Num 4)
    )
  ,
    ( "associates applications to the left"
    , "a 5 6 + 4"
    , Plus (App (App (Name "a") (Num 5)) (Num 6)) (Num 4)
    )
  ,
    ( "handles applications with parens"
    , "(a b c + 4) d * 8"
    , Times
        ( App
            ( Plus
                (App (App (Name "a") (Name "b")) (Name "c"))
                (Num 4)
            )
            (Name "d")
        )
        (Num 8)
    )
  ,
    ( "handles ternary operators"
    , "a ? b : c"
    , Cond (Name "a") (Name "b") (Name "c")
    )
  , {-
    FIXME: You have to give up ternary without spaces if you want quasi-ambiguous
    non-prefix operators (Unwrap and Cond), without a non-prefix backtrack.
    Non-prefix backtracks are really bad for error messages using parser-combinators.
    Prefix backtracks are fine.
    ,
      ( "handles ternary operators without spaces"
      , "a?b:c"
      , Cond (Name "a") (Name "b") (Name "c")
      )
    -}

    ( "handles nested ternary operators"
    , "a ? b ? c : d : e"
    , Cond
        (Name "a")
        (Cond (Name "b") (Name "c") (Name "d"))
        (Name "e")
    )
  ,
    ( "handles ternary operators with complex expressions"
    , "a + b * c 2 ? 1 + 2 : d"
    , Cond
        (Plus (Name "a") (Times (Name "b") (App (Name "c") (Num 2))))
        (Plus (Num 1) (Num 2))
        (Name "d")
    )
  ,
    ( "handles if then else"
    , "if a then b else c"
    , Cond (Name "a") (Name "b") (Name "c")
    )
  ,
    ( "handles nested if then else"
    , "if a then if b then c else d else e"
    , Cond
        (Name "a")
        (Cond (Name "b") (Name "c") (Name "d"))
        (Name "e")
    )
  ,
    ( "handles if then else with complex expressions"
    , "if a + b * c 2 then 1 + 2 else d"
    , Cond
        (Plus (Name "a") (Times (Name "b") (App (Name "c") (Num 2))))
        (Plus (Num 1) (Num 2))
        (Name "d")
    )
  ,
    ( "handles mixed if then else and ternary"
    , "if a then b ? c : d else e"
    , Cond
        (Name "a")
        (Cond (Name "b") (Name "c") (Name "d"))
        (Name "e")
    )
  ,
    ( "handles ambiguous prefix operators"
    , "(3, 4)"
    , Tuple (Num 3) (Num 4)
    )
  ,
    ( "handles tuple with whitespace"
    , " ( 3 , 4 ) "
    , Tuple (Num 3) (Num 4)
    )
  ,
    ( "handles postfix operators"
    , "3--"
    , PostDecrement (Num 3)
    )
  ,
    ( "handles complex expressions with postfix operators"
    , "a ? b + c 3-- : d"
    , Cond
        (Name "a")
        (Plus (Name "b") (App (Name "c") (PostDecrement (Num 3))))
        (Name "d")
    )
  ,
    ( "handles partially-ambiguous prefix operators mixed with non-prefix operators"
    , "3 ++a"
    , App (Num 3) (Increment (Name "a"))
    )
  ,
    ( "handles mixed infix and non-prefix with overlapping"
    , "3 + ++a-- b"
    , Plus (Num 3) (App (PostDecrement (Increment (Name "a"))) (Name "b"))
    )
  ,
    ( "handles space-sensitive operators"
    , "a? ? b : c"
    , Cond (Unwrap (Name "a")) (Name "b") (Name "c")
    )
  ,
    ( "handles custom index operator"
    , "a[0]"
    , Custom "index" [Name "a", Num 0]
    )
  ,
    ( "handles custom index operator within other expressions"
    , "2 + a[2*k]-- ? b : c"
    , Cond
        ( Plus
            (Num 2)
            (PostDecrement (Custom "index" [Name "a", Times (Num 2) (Name "k")]))
        )
        (Name "b")
        (Name "c")
    )
  ]

syntaxRuleTests =
  [
    ( "parses simple prefix rule"
    , "@increment ++_:2000"
    , ("increment", [Chunk Optional "++", Hole Forbid 2000])
    )
  ,
    ( "parses grouping rule"
    , "@group ( _:0 )"
    , ("group", [Chunk Optional "(", Hole Optional 0, Chunk Optional ")"])
    )
  ,
    ( "parses simple infix rule"
    , "@plus _:100 + _:100"
    , ("plus", [Hole Forbid 100, Chunk Optional "+", Hole Optional 100])
    )
  ,
    ( "parses simple postfix rule"
    , "@post-decrement _:200--"
    , ("post-decrement", [Hole Forbid 200, Chunk Forbid "--"])
    )
  ]

main = hspec $ do
  describe "pExpression" $ do
    forM_ tests $ \(title, input, expected) ->
      it title $ do
        parse pExpr "<test>" input `shouldParse` expected
  describe "pSyntaxRule" $ do
    forM_ syntaxRuleTests $ \(title, input, expected) ->
      it title $ do
        parse pSyntaxRule "<test>" input `shouldParse` expected
  describe "customOperators" $ do
    it "are parseable" $ do
      parse pAllSyntaxRules "<test>" `shouldSucceedOn` customOperatorsStr
