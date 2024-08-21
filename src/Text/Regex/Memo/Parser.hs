{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Text.Regex.Memo.Parser(parseRx) where

import Control.Applicative.Combinators.NonEmpty
import Data.Bifunctor
import Data.Void
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

import Text.Regex.Memo.Rx

type Parseable s = (Stream s, Token s ~ Char)

parseRx :: String -> Either String Rx
parseRx str = first errorBundlePretty $ parse topLevel "" str

topLevel :: Parseable s => Parsec Void s Rx
topLevel = alternated

alternated :: Parseable s => Parsec Void s Rx
alternated = do
  rx1 <- concatenated
  mrx2 <- optional $ char '|' *> concatenated
  case mrx2 of
    Nothing -> pure rx1
    Just rx2 -> pure $ RAlt rx1 rx2

concatenated :: Parseable s => Parsec Void s Rx
concatenated = do
  rxs <- some repeated
  pure $ foldl1 RConcat rxs

repeated :: Parseable s => Parsec Void s Rx
repeated = do
  rx <- parens topLevel <|> simpleChar
  star <- optional $ char '*'
  case star of
    Nothing -> pure rx
    Just _ -> pure $ RStar rx
  where
  simpleChar = RCh <$> noneOf "()*|"

parens :: Parseable s => Parsec Void s a -> Parsec Void s a
parens = between (char '(') (char ')')
