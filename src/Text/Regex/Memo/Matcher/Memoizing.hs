{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.Memoizing
( match
) where

import Control.Applicative hiding (empty)
import Data.ByteString.Char8 qualified as BS
import Data.EnumMap.Strict qualified as EM
import Data.EnumSet qualified as ES
import Data.Maybe

import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.NFA

newtype MemoTable q = MemoTable (EM.EnumMap Int (ES.EnumSet q))

match :: StateId q => NFA 'Unique q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = go (empty (length transitions) (BS.length bs)) initState 0
  where
  go memo q i
    | (q, i) `member` memo = Failure
    | q == finState = SuccessAt i
    | i >= BS.length bs = Failure
  go memo q i = case q `getTrans` transitions of
                  TEps q' -> go memo q' i
                  TBranch q1 q2 -> go memo q1 i <|> go (insert (q1, i) memo) q2 i
                  TCh ch q'
                   | bs `BS.index` i == ch -> go memo q' (i + 1)
                   | otherwise -> Failure


empty :: Int -> Int -> MemoTable q
empty _stateCount _strLen = MemoTable mempty

member :: StateId q => (q, Int) -> MemoTable q -> Bool
member (q, i) (MemoTable tbl) = case i `EM.lookup` tbl of
                                  Nothing -> False
                                  Just s -> q `ES.member` s

insert :: StateId q => (q, Int) -> MemoTable q -> MemoTable q
insert (q, i) (MemoTable tbl) = MemoTable $ EM.alter (Just . ES.insert q . fromMaybe mempty) i tbl
