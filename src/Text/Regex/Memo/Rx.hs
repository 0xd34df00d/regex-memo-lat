{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Regex.Memo.Rx
( Rx(..)
, RxKind(..)
, prettyRx

, desugar
) where
import Data.Kind

data RxKind = Parsed | Desugared deriving (Eq, Ord, Show)

data Rx :: RxKind -> Type where
  RCh :: Char -> Rx k
  REps :: Rx k
  RConcat :: Rx k -> Rx k -> Rx k
  RAlt :: Rx k -> Rx k -> Rx k
  RStar :: Rx k -> Rx k
  ROptional :: Rx 'Parsed -> Rx 'Parsed

deriving instance Eq (Rx k)
deriving instance Show (Rx k)

prettyRx :: Rx k -> String
prettyRx = \case
  RCh ch -> [' ', ch, ' ']
  REps -> "ε"
  RConcat r1 r2 -> prettyRx r1 <> prettyRx r2
  RAlt r1 r2 -> "(" <> prettyRx r1 <> "|" <> prettyRx r2 <> ")"
  RStar r -> "(" <> prettyRx r <> ")*"
  ROptional r -> "(" <> prettyRx r <> ")?"

desugar :: Rx 'Parsed -> Rx 'Desugared
desugar = \case
  RCh ch -> RCh ch
  REps -> REps
  RConcat r1 r2 -> RConcat (desugar r1) (desugar r2)
  RAlt r1 r2 -> RAlt (desugar r1) (desugar r2)
  RStar r -> RStar (desugar r)
  ROptional r -> RAlt (desugar r) REps
