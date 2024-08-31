{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Text.Regex.Memo.NFA
( NFA(..)
, Trans(..)
, transTargets
, NFAStage(..)

, StateId
, TransMap
, getTrans

, toWord64
, fromWord64
, maxState

, prettyNFABuilding
, prettyNFAComplete
) where

import Data.Array.Unboxed qualified as A (Array, Ix)
import Data.Array.Base qualified as A
import Data.Bits
import Data.ByteString.Internal qualified as BS
import Data.EnumMap.Strict qualified as EM
import Data.EnumSet qualified as ES
import Data.Hashable
import Data.List (sortBy)
import Data.Kind
import Data.Ord
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VG
import Data.Vector.Unboxed qualified as VU
import Data.Word
import GHC.IsList

data Trans q
  = TEps q
  | TBranch q q
  | TCh Word8 q
  deriving (Eq, Show, VU.Unbox)

instance Integral q => VU.IsoUnbox (Trans q) Word64 where
  toURepr = toWord64
  fromURepr = fromWord64

newtype instance VU.MVector s (Trans q) = MV_Trans (VU.MVector s Word64)
newtype instance VU.Vector    (Trans q) = V_Trans  (VU.Vector    Word64)

deriving via (Trans q `VU.As` Word64) instance Integral q => VG.MVector VU.MVector (Trans q)
deriving via (Trans q `VU.As` Word64) instance Integral q => VG.Vector  VU.Vector  (Trans q)

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
  TransType 'NFAComplete q = VU.Vector (Trans q)

maxState :: Integral q => q
maxState = 2 ^ 8

toWord64 :: Integral q => Trans q -> Word64
toWord64 = \case
  TEps q -> (fromIntegral q .<<. 2) .|. 0b00
  TBranch q1 q2 -> (fromIntegral q1 .<<. 18) .|. (fromIntegral q2 .<<. 2) .|. 0b01
  TCh w q -> (fromIntegral w .<<. 18) .|. (fromIntegral q .<<. 2) .|. 0b10

fromWord64 :: Integral q => Word64 -> Trans q
fromWord64 w = case w .&. 0b11 of
  0b00 -> TEps $ fromIntegral $ w .>>. 2
  0b01 -> TBranch (fromIntegral $ (w .>>. 18) .&. 0xffff) (fromIntegral $ (w .>>. 2) .&. 0xffff)
  _    -> TCh (fromIntegral $ w .>>. 18) (fromIntegral $ (w .>>. 2) .&. 0xffff)

getTrans :: StateId q => q -> TransType 'NFAComplete q -> Trans q
getTrans q m = m `VU.unsafeIndex` fromIntegral q
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

prettyNFABuilding :: (StateId q, Show q) => NFA 'NFABuilding q -> String
prettyNFABuilding NFA{..} = unlines $ ("initial: " <> show initState <> "; final: " <> show finState) :
  [ show q <> " ~> " <> prettyTrans trans
  | (q, trans) <- sortBy (comparing fst) $ toList transitions
  ]

prettyNFAComplete :: (StateId q, Show q) => NFA 'NFAComplete q -> String
prettyNFAComplete NFA{..} = unlines $ ("initial: " <> show initState <> "; final: " <> show finState) :
  [ show q <> " ~> " <> prettyTrans trans
  | (q, trans) <- zip [0 :: Int ..] $ toList transitions
  ]
