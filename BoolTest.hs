import System.Exit (exitFailure)

import Data.List

import Tuura.Boolean
import Tuura.Plato.BoolToConcept

main :: IO ()
main = do
    testAndOutput
    testOrCNF
    testOA22Output
    testAO22CNF
    testXorOutput

testAndOutput :: IO ()
testAndOutput = do
    putStrLn "=== testAndOutput"
    assertEq result expected
  where
    result   = snd $ fromFunctions "a*b" "!(a*b)" "x"
    expected = unlines ["module Concept where",
                        "",
                        "import Tuura.Concept.STG",
                        "",
                        "circuit a b x = outRise <> outFall <> interface <> initialState",
                        "  where",
                        "    outRise = rise b ~> rise x <> rise a ~> rise x",
                        "    outFall = [fall a , fall b] ~|~> fall x",
                        "    interface = inputs [a , b] <> outputs [x]",
                        "    initialState = initialise0 [a , b , x]"]

testOrCNF :: IO ()
testOrCNF = do
    putStrLn "===testOrCNF"
    assertEq (sort $ fromCNF orCNF) (sort $ fromCNF expected)
  where
    orExpr = "a+b"
    orCNF  = simplifyCNF $ convertToCNF $ right $ parseExpr orExpr
    expected = CNF [[a, b]]
    a = Literal "a" True
    b = Literal "b" True
    right (Right x) = x
    right (Left _) = right (parseExpr "")

testOA22Output :: IO ()
testOA22Output = do
    putStrLn "=== testOA22Output"
    assertEq result expected
  where
    result   = snd $ fromFunctions "(a+b)*(c+d)" "!((a+b)*(c+d))" "x"
    expected = unlines ["module Concept where",
                        "",
                        "import Tuura.Concept.STG",
                        "",
                        "circuit a b c d x = outRise <> outFall <> interface <> initialState",
                        "  where",
                        "    outRise = [rise c , rise d] ~|~> rise x <> [rise a , rise b] ~|~> rise x",
                        "    outFall = [fall b , fall d] ~|~> fall x <> [fall a , fall d] ~|~> fall x <> [fall b , fall c] ~|~> fall x <> [fall a , fall c] ~|~> fall x",
                        "    interface = inputs [a , b , c , d] <> outputs [x]",
                        "    initialState = initialise0 [a , b , c , d , x]"]



testAO22CNF :: IO ()
testAO22CNF = do
    putStrLn "===testAO22CNF"
    assertEq (sort $ fromCNF ao22CNF) (sort $ fromCNF expected)
  where
    ao22Expr = "a*b + c*d"
    ao22CNF  = simplifyCNF $ convertToCNF $ right $ parseExpr ao22Expr
    expected = CNF [[a, c], [a, d], [b, c], [b, d]]
    a = Literal "a" True
    b = Literal "b" True
    c = Literal "c" True
    d = Literal "d" True
    right (Right x) = x
    right (Left _) = right (parseExpr "")

testXorOutput :: IO ()
testXorOutput = do
    putStrLn "=== testXorOutput"
    assertEq result expected
  where
    result   = snd $ fromFunctions "(!a*b)+(a*!b)" "!((!a*b)+(a*!b))" "x"
    expected = unlines ["module Concept where",
                        "",
                        "import Tuura.Concept.STG",
                        "",
                        "circuit a b x = outRise <> outFall <> interface <> initialState",
                        "  where",
                        "    outRise = [rise a , rise b] ~|~> rise x <> [fall a , fall b] ~|~> rise x",
                        "    outFall = [rise a , fall b] ~|~> fall x <> [fall a , rise b] ~|~> fall x",
                        "    interface = inputs [a , b] <> outputs [x]",
                        "    initialState = initialise0 [a , b , x]"]

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq have need
    | need /= have = do
        putStrLn $ "--- FAIL:\nneed: " ++ show need
        putStrLn $ "have: " ++ show have
        exitFailure
    | otherwise = putStrLn $ "OK " ++ show need
