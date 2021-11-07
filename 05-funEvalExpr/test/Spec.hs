import Test.HUnit
import Control.Applicative
import Control.Monad
import Eval
import Parser

t_01 = TestCase (assertEqual "TEST01: parseChar function"
                (Just ('a', "bc")) $ runParser (parseChar 'a') "abc")

t_02 = TestCase (assertEqual "TEST02: parseAnyChar function"
                (Just ('6', "66")) $ runParser (parseAnyChar "0123456789") "666")

t_03 = TestCase (assertEqual "TEST03: Division by 0"
                Nothing (evalExpr "2/0"))

tests = TestList [ TestLabel "test_01" t_01
                 , TestLabel "test_02" t_02
                 , TestLabel "test_03" t_03
                 ]

main = do runTestTT tests