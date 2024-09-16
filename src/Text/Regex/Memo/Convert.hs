{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Text.Regex.Memo.Convert(convert) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed qualified as A
import Data.ByteString.Internal qualified as BS
import Data.EnumMap.Strict qualified as EM
import Data.EnumSet qualified as ES
import Data.Vector.Unboxed qualified as VU
import Data.Word

import Text.Regex.Memo.Rx
import Text.Regex.Memo.NFA

convert :: Rx 'Parsed -> NFA 'NFAComplete Word32
convert rxTop = finalize $ evalState (go $ desugar rxTop) 0
  where
  go :: Rx 'Desugared -> State Word32 (NFA 'NFABuilding Word32)
  go rx = do
    (q0, q1) <- allocPair
    case rx of
      RCh ch -> pure $ NFA [(q0, TCh (BS.c2w ch) q1)] q0 q1 ()
      REps -> pure $ NFA [(q0, TEps q1)] q0 q1 ()
      RConcat r1 r2 -> do
        releasePair q0 q1
        NFA t1 r1q0 r1q1 _ <- go r1
        NFA t2 r2q0 r2q1 _ <- go r2
        pure $ NFA (t1 <> t2 <> [(r1q1, TEps r2q0)]) r1q0 r2q1 ()
      RAlt r1 r2 -> do
        NFA t1 r1q0 r1q1 _ <- go r1
        NFA t2 r2q0 r2q1 _ <- go r2
        pure $ NFA (t1 <> t2 <> [(q0, TBranch r1q0 r2q0), (r1q1, TEps q1), (r2q1, TEps q1)]) q0 q1 ()
      RStar r -> do
        NFA t q0' q1' _ <- go r
        pure $ NFA (t <> [(q0, TBranch q0' q1), (q1', TEps q0)]) q0 q1 ()

  allocPair = do
    q <- get
    modify' (+ 2)
    pure (q, q + 1)
  releasePair q0 q1 = do
    q <- get
    when (q == q1 + 1 && q == q0 + 2) $ modify' (subtract 2)

  finalize :: StateId q => NFA 'NFABuilding q -> NFA 'NFAComplete q
  finalize nfa = nfa{ highIndegs, transitions = transitions', finState = finState' }
    where
    finState' = fromIntegral $ length $ transitions nfa
    transitions' = VU.fromList $ compressTransitions (finState nfa) $ transitions nfa
    highIndegs =
      ES.fromList $ EM.keys $ EM.filter (>= 2) $ EM.fromListWith (+)
        [ (q, 1 :: Int)
        | ts <- VU.toList transitions'
        , q <- transTargets ts
        ]

compressTransitions :: StateId q => q -> EM.EnumMap q (Trans q) -> [Trans q]
compressTransitions final trans = [ mapTransIds t | t <- EM.elems trans ]
  where
  transMap = EM.fromList $ (final, fromIntegral $ length trans) : zip (EM.keys trans) [0..]
  mapTransIds = \case
    TEps q -> TEps $ transMap EM.! q
    TBranch q1 q2 -> TBranch (transMap EM.! q1) (transMap EM.! q2)
    TCh c q -> TCh c (transMap EM.! q)
