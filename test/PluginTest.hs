{-# OPTIONS_GHC -fplugin=Nau.Plugin.ViewPattern -ddump-parsed-ast -ddump-rn #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit
import qualified Unused
import TestTypes (ANON(..))

-- kana: f :: HasField "a" Int r => HasField "b" Int r => r -> Int
-- kana: f ((\x -> (getField @"a" x, getField "b" x)) -> (a, b)) = a + b :: Int
f ANON{a, b} = a + b :: Int

f0 ANON{} = ()

f1 :: ANON -> Int
f1 ANON{a = p1} = p1
-- !(getField @"f1" -> p1)

f2 :: ANON -> Int
f2 ANON{a = p1, b = p2} = p1 + p1 :: Int
-- -!((\x -> (getField @"f1" x, getField @"f2")) -> (p1, p2))

f3 ANON{a} = a

f4 ANON{a, b} = a + b :: Int

-- Actually typechecking is enough, but we do silly runtime test as well
hasFieldTests :: Assertion
hasFieldTests = do
  assertEqual "f1" 2 $ f1 $ ANON{a=2, b = 3}
  -- assertEqual "f2" 5 $ f2 $ ANON{a=2, b = 3}

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
    testGroup "Sanity" [
      testCase "HasField" hasFieldTests
    ],
    Unused.unusedTests
  ]

