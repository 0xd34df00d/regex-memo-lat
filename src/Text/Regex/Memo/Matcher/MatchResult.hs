{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.MatchResult(MatchResult(..)) where

import Control.Applicative
import GHC.Generics
import Control.DeepSeq

data MatchResult a = SuccessAt a | Failure
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (NFData)

instance Applicative MatchResult where
  SuccessAt f <*> SuccessAt v = SuccessAt $ f v
  _           <*> _           = Failure
  pure = SuccessAt

instance Alternative MatchResult where
  SuccessAt a <|> ~_ = SuccessAt a
  _           <|> ~r = r
  empty = Failure
