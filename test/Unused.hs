{-# OPTIONS_GHC -fplugin=Nau.Plugin.ViewPattern -Werror -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{- This test is compile only, ensuring that we don't inject GHC.Records.Compat import unless it really used
-}
module Unused where

import Test.Tasty

unusedTests = testGroup "unused" []
