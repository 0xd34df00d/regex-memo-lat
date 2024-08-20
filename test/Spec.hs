{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString qualified as BS
import Data.Either
import Test.Hspec

import Text.Regex.Memo
import Text.Regex.Memo.Parser
import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.Matcher.Naive

main :: IO ()
main = hspec $ do
  describe "Rx parser" $
    it "parses a basic regex" $
      parseRx "a(b|c)d(e|f)*z" `shouldSatisfy` isRight
  describe "Smoke tests (naive)" $ do
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
