{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Either
import Test.Hspec
import Test.QuickCheck hiding (Failure)

import Text.Regex.Memo
import Text.Regex.Memo.Parser
import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.Matcher.Naive qualified as N
import Text.Regex.Memo.Matcher.Memoizing qualified as M

main :: IO ()
main = hspec $ do
  describe "Rx parser" $
    it "parses a basic regex" $
      parseRx "a(b|c)d(e|f)*z" `shouldSatisfy` isRight
  describe "Smoke tests (naive)" $ smokes N.match
  describe "Smoke tests (memo)" $ smokes M.match
  describe "Naive and memo agree" $ do
    it "on arbitrary input" $ do
      let Right rx = parseRx "a(b|c)d(e|f)*z"
      let nfa = convert rx
      property $ \str -> let bs = BS.pack $ getASCIIString str in M.match nfa bs == N.match nfa bs
  where
  smokes match = do
    it "matches what it should" $ do
      Right rx <- pure $ parseRx "a(b|c)d(e|f)*z"
      let extra = "we don't care about the rest"
      forM_ ["abdz", "acdz", "abdeffefefez"] $ \str ->  do
        match (convert rx) str            `shouldBe` SuccessAt (BS.length str)
        match (convert rx) (str <> extra) `shouldBe` SuccessAt (BS.length str)
    it "doesn't match what it shouldn't" $ do
      Right rx <- pure $ parseRx "a(b|c)d(e|f)*z"
      let str = "abdeffefefe"
      match (convert rx) str `shouldBe` Failure
