{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Text.Regex.Memo.Matcher.Naive
( match
, matchSimplest
) where

import Control.Applicative
import Control.Monad.ST.Strict
import Data.ByteString qualified as BS
import Data.Vector.Unboxed.Mutable qualified as VM

import Text.Regex.Memo.Matcher.MatchResult
import Text.Regex.Memo.NFA

data Stack s a = Stack
  { theVec :: VM.MVector s a
  , size :: Int
  }

mkStack :: VM.Unbox a => Int -> ST s (Stack s a)
mkStack initSize = (`Stack` 0) <$> VM.unsafeNew initSize

isEmpty :: Stack s a -> Bool
isEmpty = (== 0) . size

push :: VM.Unbox a => a -> Stack s a -> ST s (Stack s a)
push a Stack{..} = do
  vec' <- if size /= VM.length theVec
             then pure theVec
             else theVec `VM.unsafeGrow` size
  VM.unsafeWrite vec' size a
  pure $ Stack vec' (size + 1)

pop :: VM.Unbox a => Stack s a -> ST s (a, Stack s a)
pop Stack{..} = do
  a <- VM.unsafeRead theVec (size - 1)
  pure (a, Stack theVec (size - 1))

match :: (VM.Unbox q, StateId q) => NFA 'NFAComplete q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = runST $ do
  stack <- mkStack $ BS.length bs * 2
  let go s q i
        | q == finState = pure $ SuccessAt i
        | otherwise = case q `getTrans` transitions of
              TEps q' -> go s q' i
              TBranch q1 q2 -> do stack' <- (q2, i) `push` s
                                  go stack' q1 i
              TCh ch q'
                | bs `BS.indexMaybe` i == Just ch -> go s q' (i + 1)
                | isEmpty s -> pure Failure
                | otherwise -> do ((q'', i''), stack') <- pop s
                                  go stack' q'' i''
  go stack initState 0

matchSimplest :: StateId q => NFA 'NFAComplete q -> BS.ByteString -> MatchResult Int
matchSimplest NFA{..} bs = go initState 0
  where
  go q i
    | q == finState = SuccessAt i
  go q i = case q `getTrans` transitions of
             TEps q' -> go q' i
             TBranch q1 q2 -> go q1 i <|> go q2 i
             TCh ch q'
               | bs `BS.indexMaybe` i == Just ch -> go q' (i + 1)
               | otherwise -> Failure
