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
, NFAStage(..)

, StateId
, TransMap
, getTrans

, prettyNFA
) where

import Data.Array qualified as A
import Data.Array.Base qualified as A
import Data.ByteString.Internal qualified as BS
import Data.EnumMap.Strict qualified as EM
import Data.EnumSet qualified as ES
import Data.Hashable
import Data.List (sortBy)
import Data.Kind
import Data.Ord
import Data.Word
import GHC.IsList

data Trans q
  = TEps q
  | TBranch q q
  | TCh Word8 q
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
  TCh c q -> ['\'', BS.w2c c, '\'', ' '] <> show q


type StateId q = (Hashable q, Integral q, A.Ix q)


type TransMap q = EM.EnumMap q (Trans q)

data NFAStage = NFABuilding | NFAComplete

type family IndegsType (s :: NFAStage) (q :: Type) :: Type where
  IndegsType 'NFABuilding _ = ()
  IndegsType 'NFAComplete q = ES.EnumSet q

type family TransType (s :: NFAStage) (q :: Type) :: Type where
  TransType 'NFABuilding q = TransMap q
  TransType 'NFAComplete q = A.Array q (Trans q)

getTrans :: StateId q => q -> TransType 'NFAComplete q -> Trans q
getTrans q m = m `A.unsafeAt` fromIntegral q
{-# INLINE getTrans #-}

data NFA stage q = NFA
  { transitions :: TransType stage q
  , initState :: q
  , finState :: q
  , highIndegs :: IndegsType stage q
  }

instance Enum k => IsList (EM.EnumMap k v) where
  type Item (EM.EnumMap k v) = (k, v)
  fromList = EM.fromList
  toList = EM.toList

prettyNFA :: (StateId q, Show q) => NFA 'NFABuilding q -> String
prettyNFA NFA{..} = unlines $ ("initial: " <> show initState <> "; final: " <> show finState) :
  [ show q <> " ~> " <> prettyTrans trans
  | (q, trans) <- sortBy (comparing fst) $ toList transitions
  ]
