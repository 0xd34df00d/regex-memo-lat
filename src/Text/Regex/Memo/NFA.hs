{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Regex.Memo.NFA
( NFA(..)
, Trans(..)
, transTargets
, FinStateKind(..)

, StateId
, TransMap
, getTrans

, prettyNFA
) where

import Data.EnumMap.Strict qualified as EM
import Data.EnumSet qualified as ES
import Data.Hashable
import Data.List (sortBy)
import Data.Kind
import Data.Ord
import GHC.IsList

data Trans q
  = TEps q
  | TBranch q q
  | TCh Char q
  deriving (Eq, Show)

transTargets :: Trans q -> [q]
transTargets = \case
  TEps q -> [q]
  TBranch q1 q2 -> [q1, q2]
  TCh _ q -> [q]

prettyTrans :: Show q => Trans q -> String
prettyTrans = \case
  TEps q -> "(ε) " <> show q
  TBranch q1 q2 -> show q1 <> " | " <> show q2
  TCh c q -> ['\'', c, '\'', ' '] <> show q


type StateId q = (Eq q, Hashable q, Enum q)


type TransMap q = EM.EnumMap q (Trans q)

getTrans :: StateId q => q -> TransMap q -> Trans q
getTrans q m = case q `EM.lookup` m of
                 Just r -> r
                 Nothing -> error "invariant failure"


data FinStateKind = Unique | Many

type family FinStateMod (k :: FinStateKind) (q :: Type) :: Type where
  FinStateMod 'Unique q = q
  FinStateMod 'Many q = [q]

data NFA fsk q = NFA
  { transitions :: TransMap q
  , initState :: q
  , finState :: FinStateMod fsk q
  , highIndegs :: ES.EnumSet q
  }

instance Enum k => IsList (EM.EnumMap k v) where
  type Item (EM.EnumMap k v) = (k, v)
  fromList = EM.fromList
  toList = EM.toList

prettyNFA :: (StateId q, Ord q, Show q, Show (FinStateMod fsk q)) => NFA fsk q -> String
prettyNFA NFA{..} = unlines $ ("initial: " <> show initState <> "; final: " <> show finState) :
  [ show q <> " ~> " <> prettyTrans trans
  | (q, trans) <- sortBy (comparing fst) $ toList transitions
  ]
