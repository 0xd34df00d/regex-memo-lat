{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.Memoizing
( match
) where

import Control.Applicative
import Data.ByteString.Char8 qualified as BS
import Data.HashSet qualified as HS

import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.NFA

match :: StateId q => NFA 'Unique q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = go mempty initState 0
  where
  go memo q i
    | (q, i) `HS.member` memo = Failure
    | q == finState = SuccessAt i
    | i >= BS.length bs = Failure
  go memo q i = case q `getTrans` transitions of
                  TEps q' -> go memo q' i
                  TBranch q1 q2 -> go memo q1 i <|> go (HS.insert (q1, i) memo) q2 i
                  TCh ch q'
                   | bs `BS.index` i == ch -> go memo q' (i + 1)
                   | otherwise -> Failure
