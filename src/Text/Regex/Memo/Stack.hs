{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Memo.Stack
( Stack(..)
, mkStack
, isEmpty
, push
, pop
) where

import Control.Monad.ST.Strict
import Data.Vector.Unboxed.Mutable qualified as VM

data Stack s a = Stack
  { theVec :: VM.MVector s a
  , size :: Int
  }

mkStack :: VM.Unbox a => Int -> ST s (Stack s a)
mkStack initSize = (`Stack` 0) <$> VM.unsafeNew initSize

isEmpty :: Stack s a -> Bool
isEmpty Stack{..} = size == 0

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
