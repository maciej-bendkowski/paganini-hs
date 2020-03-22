{-# OPTIONS_GHC -fno-warn-type-defaults
                -fplugin BinderAnn.Monadic
                -fplugin-opt BinderAnn.Monadic:infix=@@ #-}

import           Prelude           hiding (seq)

import           BinderAnn.Monadic

import           Control.Monad

import           Data.Paganini

import           Test.Tasty
import           Test.Tasty.HUnit

assertApprox :: (Ord a, Num a, Show a) => String -> a -> a -> a -> IO ()
assertApprox msg target val eps
  = unless (abs (target - val) < eps) (assertFailure msg')
  where msg' = msg ++ ": expected " ++ show target ++ " was " ++ show val

assertEqualList :: (Ord t, Num t, Show t) => String -> t -> [Maybe t] -> [Maybe t] -> IO ()
assertEqualList _ _ [] [] = return ()
assertEqualList msg eps (Just x : xs) (Just y : ys)
  = assertApprox msg x y eps >> assertEqualList msg eps xs ys

assertEqualList msg _ xs ys
  = assertFailure $ msg ++ ": expected " ++ show xs ++ ", was " ++ show ys

testPaganini :: Show a => (t -> IO b) -> IO (Either a t) -> IO b
testPaganini testCase' comp = do
  x <- comp
  case x of
    Left err -> assertFailure (show err)
    Right z  -> testCase' z

testOutput :: (Show a, Ord t, Num t, Show t) => String -> t -> [Maybe t] -> IO (Either a [Maybe t]) -> IO ()
testOutput msg eps out = testPaganini testCase'
  where testCase' = assertEqualList msg eps out

testBinTrees :: TestTree
testBinTrees = testCase "Binary trees" $
  testOutput "testBinTrees" eps expected testBinTrees'
  where eps = 1.0e-5
        expected = map Just [0.25, 2.0]

testBinTrees' :: IO (Either PaganiniError [Maybe Double])
testBinTrees' = paganini @@ do
  Let z <- variable' 100000
  Let b <- variable

  b .=. 1 + z * b^2
  tune b

  z' <- value z
  b' <- value b
  return [z',b']

testMotzkinTrees :: TestTree
testMotzkinTrees = testCase "Motzkin trees" $
  testOutput "testMotzkinTrees" eps expected testMotzkinTrees'
  where eps = 1.0e-5
        expected = map Just [0.333333, 1.0]

testMotzkinTrees' :: IO (Either PaganiniError [Maybe Double])
testMotzkinTrees' = paganini @@ do
  Let z <- variable' 200000
  Let m <- variable

  m .=. z + z * m + z * m^2
  tune m

  z' <- value z
  m' <- value m
  return [z',m']

testSimpleTrees :: TestTree
testSimpleTrees = testCase "Simply-generated trees" $
  testOutput "testSimpleTrees" eps expected testSimpleTrees'
  where eps = 1.0e-5
        expected = map Just [0.25, 0.5]

testSimpleTrees' :: IO (Either PaganiniError [Maybe Double])
testSimpleTrees' = paganini @@ do
  Let z <- variable' 100000
  Let t <- variable

  Def s <- seq t
  t .=. z * s
  tune t

  z' <- value z
  t' <- value t
  return [z',t']

testMinusConstant :: TestTree
testMinusConstant = testCase "Trees with subtraction" $
  testOutput "testMinusConstant" eps expected testMinusConstant'
  where eps = 1.0e-4
        expected = map Just [0.5]

testMinusConstant' :: IO (Either PaganiniError [Maybe Double])
testMinusConstant' = paganini @@ do
  Let z <- variable' 50000
  Let t <- variable
  Def s <- seq (2 * z)
  t .=. s - 1

  tune t
  z' <- value z
  return [z']

testTernaryTrees :: TestTree
testTernaryTrees = testCase "Ternary trees" $
  testOutput "testTernaryTrees" eps expected testTernaryTrees'
  where eps = 1.0e-5
        expected = map Just [0.148148148]

testTernaryTrees' :: IO (Either PaganiniError [Maybe Double])
testTernaryTrees' = paganini @@ do
  Let z <- variable' 100000
  Let t <- variable
  Def s <- seq' t (Equal 3)

  t .=. 1 + z * s

  tune t
  z' <- value z
  return [z']

testOtterTrees :: TestTree
testOtterTrees = testCase "Otter trees" $
  testOutput "testOtterTrees" eps expected testOtterTrees'
  where eps = 1.0e-5
        expected = map Just [0.4026975]

testOtterTrees' :: IO (Either PaganiniError [Maybe Double])
testOtterTrees' = paganini @@ do
  Let z <- variable' 1000000
  Let t <- variable
  Def m <- mset' t (Equal 2)

  t .=. 1 + z * m

  tune t
  z' <- value z
  return [z']

testLambdaTerms :: TestTree
testLambdaTerms = testCase "Lambda terms" $
  testOutput "testLambdaTerms" eps expected testLambdaTerms'
  where eps = 1.0e-4
        expected = map Just [0.24482714
                            ,1.78303233
                            ,1.15073912]

testLambdaTerms' :: IO (Either PaganiniError [Maybe Double])
testLambdaTerms' = paganini @@ do
  Let z <- variable' 100000
  Let u <- variable' 40000

  Let l <- variable
  Let d <- variable

  l .=. d + u*z*l + z*l^2
  d .=. z + z*d

  tune l
  z' <- value z
  u' <- value u
  l' <- value l
  return [z',u',l']

testCyclicDecompositions :: TestTree
testCyclicDecompositions = testCase "Cyclic decompositions" $
  testOutput "testCyclicDecompositions" eps expected testCyclicDecompositions'
  where eps = 1.0e-5
        expected = map Just [0.4057656592]

testCyclicDecompositions' :: IO (Either PaganiniError [Maybe Double])
testCyclicDecompositions' = paganini @@ do
  Let z <- variable' 20
  Let c <- variable

  Def s <- seq z
  Def u <- ucyc' (z * s) (Equal 12)
  c .=. u

  tune c
  z' <- value z
  return [z']

testUrns :: TestTree
testUrns = testCase "Urns" $
  testOutput "testUrns" eps expected testUrns'
  where eps = 1.0e-5
        expected = map Just [9]

testUrns' :: IO (Either PaganiniError [Maybe Double])
testUrns' = paganini @@ do
  Let z <- variable' 18
  Let u <- variable

  Def s <- set (2 * z)
  u .=. s

  tune u
  z' <- value z
  return [z']

testCircularGraphs :: TestTree
testCircularGraphs = testCase "Circular graphs" $
  testOutput "testCircularGraphs" eps expected testCircularGraphs'
  where eps = 1.0e-5
        expected = map Just [1.129759512]

testCircularGraphs' :: IO (Either PaganiniError [Maybe Double])
testCircularGraphs' = paganini @@ do
  Let z <- variable' 10
  Let c <- variable

  Def cp <- cyc z -- c' is not a proper Python identifier.
  c .=. cp

  tune c
  z' <- value z
  return [z']

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests
  = testGroup "Unit tests"
    [testBinTrees
    ,testMotzkinTrees
    ,testSimpleTrees
    ,testMinusConstant
    ,testTernaryTrees
    ,testOtterTrees
    ,testLambdaTerms
    ,testCyclicDecompositions
    ,testUrns
    ,testCircularGraphs]
