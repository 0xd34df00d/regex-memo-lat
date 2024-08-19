{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Regex.Memo where

import Control.Monad.State.Strict
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.List (sortBy)
import Data.Word
import Data.Ord

data Rx
  = RCh Char
  | REps
  | RConcat Rx Rx
  | RAlt Rx Rx
  | RStar Rx
  deriving (Eq, Show)

prettyRx :: Rx -> String
prettyRx = \case
  RCh ch -> [ch]
  REps -> "ε"
  RConcat r1 r2 -> prettyRx r1 <> prettyRx r2
  RAlt r1 r2 -> "(" <> prettyRx r1 <> ")|(" <> prettyRx r2 <> ")"
  RStar r -> "(" <> prettyRx r <> ")*"

data Trans q
  = TEps q
  | TBranch q q
  | TCh Char q
  deriving (Eq, Show)

prettyTrans :: Show q => Trans q -> String
prettyTrans = \case
  TEps q -> "(ε) " <> show q
  TBranch q1 q2 -> show q1 <> " | " <> show q2
  TCh c q -> ['\'', c, '\'', ' '] <> show q

data FinStateKind = Unique | Many

type family FinStateMod (k :: FinStateKind) (q :: Type) :: Type where
  FinStateMod 'Unique q = q
  FinStateMod 'Many q = [q]

data NFA fsk q = NFA
  { transitions :: HM.HashMap q (Trans q)
  , initState :: q
  , finState :: FinStateMod fsk q
  }

prettyNFA :: (Ord q, Show q, Show (FinStateMod fsk q)) => NFA fsk q -> String
prettyNFA NFA{..} = unlines $ ("initial: " <> show initState <> "; final: " <> show finState) :
  [ show q <> " ~> " <> prettyTrans trans
  | (q, trans) <- sortBy (comparing fst) $ HM.toList transitions
  ]

convert :: Rx -> NFA 'Unique Word32
convert rxTop = evalState (go rxTop) 0
  where
  go rx = do
    (q0, q1) <- allocStates
    case rx of
      RCh ch -> pure $ NFA [(q0, TCh ch q1)] q0 q1
      REps -> pure $ NFA [(q0, TEps q1)] q0 q1
      RConcat r1 r2 -> do
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

  allocStates = do
    q0 <- get
    modify' (+ 2)
    pure (q0, q0 + 1)
