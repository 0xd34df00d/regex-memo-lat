{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

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

match :: (VM.Unbox q, StateId q) => NFA 'NFAComplete q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = runST $ do
  stack <- VM.unsafeNew 24_000_000
  let go s q i
        | q == finState = pure $ SuccessAt i
        | otherwise = case q `getTrans` transitions of
              TEps q' -> go s q' i
              TBranch q1 q2 -> do VM.unsafeWrite stack s (q2, i)
                                  go (s + 1) q1 i
              TCh ch q'
                | bs `BS.indexMaybe` i == Just ch -> go s q' (i + 1)
                | s == 0 -> pure Failure
                | otherwise -> do (q'', i'') <- VM.unsafeRead stack (s - 1)
                                  go (s - 1) q'' i''
  go 0 initState 0

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
