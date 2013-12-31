{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module TestSuite where

import Test.Hspec
import Test.HUnit

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Debug.Trace

--
-- What we're actually testing.
--

import System.Marquise

suite :: Spec
suite = undefined
