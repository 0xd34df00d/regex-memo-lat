{-# LANGUAGE LambdaCase #-}

module Text.Regex.Memo.Rx
( Rx(..)
, prettyRx
) where

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
