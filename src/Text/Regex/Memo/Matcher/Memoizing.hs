{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.Memoizing(match) where

import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString.Char8 qualified as BS
import Data.EnumMap.Strict qualified as EM
import Data.EnumSet qualified as ES
import Data.Maybe

import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.NFA

newtype MemoTable q = MemoTable (EM.EnumMap Int (ES.EnumSet q))

match :: StateId q => NFA 'NFAComplete q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = evalState (go initState 0) (empty (length transitions) (BS.length bs))
  where
  len = BS.length bs

  go q i
    | q == finState = pure $ SuccessAt i
    | i >= len = pure Failure
  go q i = do
    memo <- get
    if (q, i) `member` memo
       then pure Failure
       else do
         res <- case q `getTrans` transitions of
                  TEps q' -> go q' i
                  TBranch q1 q2 -> do
                    r1 <- go q1 i
                    case r1 of
                      SuccessAt j -> pure $ SuccessAt j
                      Failure -> go q2 i
                  TCh ch q'
                   | bs `BS.index` i == ch -> go q' (i + 1)
                   | otherwise -> pure Failure
         when (res == Failure && q `ES.member` highIndegs) $ modify' $ insert (q, i)
         pure res


empty :: Int -> Int -> MemoTable q
empty _stateCount _len = MemoTable mempty

member :: StateId q => (q, Int) -> MemoTable q -> Bool
member (q, i) (MemoTable tbl) = case i `EM.lookup` tbl of
                                  Nothing -> False
                                  Just s -> q `ES.member` s

insert :: StateId q => (q, Int) -> MemoTable q -> MemoTable q
insert (q, i) (MemoTable tbl) = MemoTable $ EM.alter (Just . ES.insert q . fromMaybe mempty) i tbl
