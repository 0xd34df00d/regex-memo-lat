{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 qualified as BS
import Criterion.Main

import Text.Regex.Memo
import Text.Regex.Memo.Parser
import Text.Regex.Memo.Matcher.Naive qualified as N
import Text.Regex.Memo.Matcher.Memoizing qualified as M

main :: IO ()
main = do
  Right nfa <- pure $ convert <$> parseRx "(aa|ab)*z"
  Right dos <- pure $ convert <$> parseRx "(a|a)*z"
  defaultMain
    [ bgroup "naive matcher" $
      [ bench (show len <> " pat " <> show pat <> " end " <> show end ) $ nf (N.match nfa) $ BS.concat (replicate len pat) <> end
      | len <- lens
      , pat <- ["aa", "ab"]
      , end <- ["", "z"]
      ]
    , bgroup "memo matcher" $
      [ bench (show len <> " pat " <> show pat <> " end " <> show end ) $ nf (M.match nfa) $ BS.concat (replicate len pat) <> end
      | len <- lens
      , pat <- ["aa", "ab"]
      , end <- ["", "z"]
      ]
    , bgroup "dos (naive)" $
      [ bench (show len) $ nf (N.match dos) $ BS.replicate len 'a' <> "w"
      | len <- dosLens
      ]
    , bgroup "dos (memo)" $
      [ bench (show len) $ nf (M.match dos) $ BS.replicate len 'a' <> "w"
      | len <- dosLens
      ]
    ]
  where
  lens = [100_000]
  dosLens = [10, 12 .. 20]
