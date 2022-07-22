{-# OPTIONS_GHC -fplugin=Data.Record.Plugin.HasFieldPattern -Werror -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{- This test is compile only, ensuring that we don't inject GHC.Records.Compat import unless it really used
-}
module Unused where

import Test.Tasty

unusedTests = testGroup "unused" []
