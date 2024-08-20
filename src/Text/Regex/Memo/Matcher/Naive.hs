{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.Naive
( MatchResult(..)
, match
) where

import Control.Applicative
import Data.ByteString.Char8 qualified as BS

import Text.Regex.Memo.NFA

data MatchResult a = SuccessAt a | Failure deriving (Eq, Ord, Show, Functor)

instance Applicative MatchResult where
  SuccessAt f <*> SuccessAt v = SuccessAt $ f v
  _           <*> _           = Failure
  pure = SuccessAt

instance Alternative MatchResult where
  SuccessAt a <|> _ = SuccessAt a
  _           <|> r = r
  empty = Failure

match :: StateId q => NFA 'Unique q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = go initState 0
  where
  go q i
    | q == finState = SuccessAt i
    | i >= BS.length bs = Failure
  go q i = case q `getTrans` transitions of
             TEps q' -> go q' i
             TBranch q1 q2 -> go q1 i <|> go q2 i
             TCh ch q'
               | bs `BS.index` i == ch -> go q' (i + 1)
               | otherwise -> Failure
