{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Regex.Memo.NFA
( NFA(..)
, Trans(..)
, FinStateKind(..)
, prettyNFA
) where

import Data.HashMap.Strict qualified as HM
import Data.List (sortBy)
import Data.Kind
import Data.Ord

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

