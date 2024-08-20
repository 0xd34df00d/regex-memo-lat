{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.MatchResult(MatchResult(..)) where

import Control.Applicative

data MatchResult a = SuccessAt a | Failure deriving (Eq, Ord, Show, Functor)

instance Applicative MatchResult where
  SuccessAt f <*> SuccessAt v = SuccessAt $ f v
  _           <*> _           = Failure
  pure = SuccessAt

instance Alternative MatchResult where
  SuccessAt a <|> ~_ = SuccessAt a
  _           <|> ~r = r
  empty = Failure
