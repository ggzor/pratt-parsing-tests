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
  ,
    ( "handles ternary operators without spaces"
    , "a?b:c"
    , Cond (Name "a") (Name "b") (Name "c")
    )
  ,
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
  ]

main = hspec $ do
  describe "pExpression" $ do
    forM_ tests $ \(title, input, expected) ->
      it title $ do
        parse pExpr "<test>" input `shouldParse` expected
