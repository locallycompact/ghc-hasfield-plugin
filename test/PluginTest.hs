{-# OPTIONS_GHC -fplugin=Nau.Plugin.ViewPattern -ddump-parsed-ast -ddump-rn #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module PluginTest where
import GHC.Generics
import GHC.Records.Compat

data REC = REC { a :: Int, b :: Int } deriving (Show, Generic)

-- kana: f :: HasField "a" Int r => HasField "b" Int r => r -> Int
-- kana: f ((\x -> (getField @"a" x, getField "b" x)) -> (a, b)) = a + b :: Int
f REC{a, b} = a + b :: Int

f0 REC{} = ()

f1 REC{a = p1} = p1
-- !(getField @"f1" -> p1)

f2 REC{a = p1, b = p2} = p1 + p1 :: Int
-- -!((\x -> (getField @"f1" x, getField @"f2")) -> (p1, p2))

f3 REC{a} = a

f4 REC{a, b} = a + b :: Int
