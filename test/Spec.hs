{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as BS
import Data.Either
import Test.Hspec

import Text.Regex.Memo
import Text.Regex.Memo.Parser
import Text.Regex.Memo.Matcher.Naive

main :: IO ()
main = hspec $ do
  describe "Rx parser" $
    it "parses a basic regex" $
      parseRx "a(b|c)d(e|f)*z" `shouldSatisfy` isRight
  describe "Smoke tests (naive)" $ do
    it "matches what it should" $ do
      Right rx <- pure $ parseRx "a(b|c)d(e|f)*z"
      let nfa = convert rx
      let str = "abdeffefefez"
      match nfa str `shouldBe` SuccessAt (BS.length str)
