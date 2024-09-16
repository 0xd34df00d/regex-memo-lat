{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Memo.Matcher.Naive
( match
, matchSimplest
) where

import Control.Applicative
import Control.Monad.ST.Strict
import Data.ByteString qualified as BS
import Data.Vector.Unboxed.Mutable qualified as VM

import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.NFA
import Text.Regex.Memo.Stack

match :: (VM.Unbox q, StateId q) => NFA 'NFAComplete q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = runST $ do
  stack <- mkStack $ BS.length bs * 2
  let go s q i
        | q == finState = pure $ SuccessAt i
        | otherwise = case q `getTrans` transitions of
              TEps q' -> go s q' i
              TBranch q1 q2 -> do stack' <- (q2, i) `push` s
                                  go stack' q1 i
              TCh ch q'
                | bs `BS.indexMaybe` i == Just ch -> go s q' (i + 1)
                | isEmpty s -> pure Failure
                | otherwise -> do ((q'', i''), stack') <- pop s
                                  go stack' q'' i''
  go stack initState 0

matchSimplest :: StateId q => NFA 'NFAComplete q -> BS.ByteString -> MatchResult Int
matchSimplest NFA{..} bs = go initState 0
  where
  go q i
    | q == finState = SuccessAt i
  go q i = case q `getTrans` transitions of
             TEps q' -> go q' i
             TBranch q1 q2 -> go q1 i <|> go q2 i
             TCh ch q'
               | bs `BS.indexMaybe` i == Just ch -> go q' (i + 1)
               | otherwise -> Failure
