{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Text.Regex.Memo where

import Control.Monad
import Control.Monad.State.Strict
import Data.Word

import Text.Regex.Memo.Rx
import Text.Regex.Memo.NFA

convert :: Rx -> NFA 'Unique Word32
convert rxTop = evalState (go rxTop) 0
  where
  go rx = do
    (q0, q1) <- allocPair
    case rx of
      RCh ch -> pure $ NFA [(q0, TCh ch q1)] q0 q1
      REps -> pure $ NFA [(q0, TEps q1)] q0 q1
      RConcat r1 r2 -> do
        releasePair q0 q1
        NFA t1 r1q0 r1q1 <- go r1
        NFA t2 r2q0 r2q1 <- go r2
        pure $ NFA (t1 <> t2 <> [(r1q1, TEps r2q0)]) r1q0 r2q1
      RAlt r1 r2 -> do
        NFA t1 r1q0 r1q1 <- go r1
        NFA t2 r2q0 r2q1 <- go r2
        pure $ NFA (t1 <> t2 <> [(q0, TBranch r1q0 r2q0), (r1q1, TEps q1), (r2q1, TEps q1)]) q0 q1
      RStar r -> do
        NFA t q0' q1' <- go r
        pure $ NFA (t <> [(q0, TBranch q0' q1), (q1', TEps q0)]) q0 q1

  allocPair = do
    q <- get
    modify' (+ 2)
    pure (q, q + 1)
  releasePair q0 q1 = do
    q <- get
    when (q == q1 + 1 && q == q0 + 2) $ modify' (subtract 2)
