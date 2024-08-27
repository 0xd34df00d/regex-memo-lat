{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Either
import Test.Hspec
import Test.QuickCheck hiding (Failure)

import Text.Regex.Memo hiding (match)
import Text.Regex.Memo.Matcher.Naive qualified as N
import Text.Regex.Memo.Matcher.Memoizing qualified as M

main :: IO ()
main = hspec $ do
  describe "Rx parser" $
    forM_ rxs $ \(rx, _, _) ->
      it ("parses a regex: "<> rx) $ do
        parseRx rx `shouldSatisfy` isRight
  describe "Smoke tests (naive)" $ smokes N.match
  describe "Smoke tests (memo)" $ smokes M.match
  describe "Naive and memo agree" $ do
    it "on arbitrary input" $ do
      let Right rx = parseRx "a(b|c)d(e|f)*z"
      let nfa = convert rx
      property $ \str -> let bs = BS.pack $ getASCIIString str in M.match nfa bs == N.match nfa bs
  where
  smokes match = forM_ rxs $ \(rx, positive, negative) -> do
    it (rx <> " matches what it should") $ do
      Right nfa <- pure $ convert <$> parseRx rx
      let extra = "we don't care about the rest"
      forM_ positive $ \str ->  do
        match nfa str            `shouldBe` SuccessAt (BS.length str)
        match nfa (str <> extra) `shouldBe` SuccessAt (BS.length str)
    it (rx <> " doesn't match what it shouldn't") $ do
      Right nfa <- pure $ convert <$> parseRx rx
      forM_ negative $ \str ->
        match nfa str `shouldBe` Failure

  rxs = [ ("a(b|c)d(e|f)*z", ["abdz", "acdz", "abdeffefefez"], ["abdeffefefe"])
        , ("(aa|ab)*z", ["aaz", "aaabz", "abaaz"], ["aaaz"])
        , ("(aa|ab)?z", ["aaz", "z", "abz"], ["az", "aaabz"])
        , ("(aa|ab){3}z", ["aaabaaz", "aaaaaaz", "abababz"], ["aaz", "aaabz", "aaaaaaaaz"])
        , ("(aa|ab){3,5}z", ["aaabaaz", "aaaaaaz", "abababz", "aaaaaaaaz", "aaabaaaaz", "aaabaaabaaz"], ["aaz", "aaabz", "aaaaz", "aaaaaaaaaaaz", "aaaaaaaaaaaaz"])
        ]
