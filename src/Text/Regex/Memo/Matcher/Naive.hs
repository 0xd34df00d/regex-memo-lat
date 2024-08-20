{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.Naive
( MatchResult(..)
, match
) where

import Control.Applicative
import Data.ByteString.Char8 qualified as BS

import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.NFA

match :: StateId q => NFA 'Unique q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = go initState 0
  where
  go q i
    | q == finState = SuccessAt i
    | i >= BS.length bs = Failure
  go q i = case q `getTrans` transitions of
             TEps q' -> go q' i
             TBranch q1 q2 -> go q1 i <|> go q2 i
             TCh ch q'
               | bs `BS.index` i == ch -> go q' (i + 1)
               | otherwise -> Failure
