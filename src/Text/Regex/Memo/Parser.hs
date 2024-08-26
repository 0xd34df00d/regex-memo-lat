{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Text.Regex.Memo.Parser(parseRx) where

import Control.Applicative.Combinators.NonEmpty
import Data.Bifunctor
import Data.Void
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

import Text.Regex.Memo.Rx

type Parseable s = (Stream s, Token s ~ Char)

parseRx :: String -> Either String (Rx 'Parsed)
parseRx str = first errorBundlePretty $ parse topLevel "" str

topLevel :: Parseable s => Parsec Void s (Rx 'Parsed)
topLevel = alternated

alternated :: Parseable s => Parsec Void s (Rx 'Parsed)
alternated = do
  rx1 <- concatenated
  mrx2 <- optional $ char '|' *> concatenated
  case mrx2 of
    Nothing -> pure rx1
    Just rx2 -> pure $ RAlt rx1 rx2

concatenated :: Parseable s => Parsec Void s (Rx 'Parsed)
concatenated = do
  rxs <- some postModified
  pure $ foldl1 RConcat rxs

postModified :: Parseable s => Parsec Void s (Rx 'Parsed)
postModified = do
  rx <- parens topLevel <|> simpleChar <|> escapedChar
  star <- optional $ char '*' <|> char '?'
  case star of
    Nothing -> pure rx
    Just '*' -> pure $ RStar rx
    Just '?' -> pure $ ROptional rx
    Just _ -> fail "unexpected char"
  where
  simpleChar = RCh <$> noneOf specialChars
  escapedChar = RCh <$> (char '\\' *> oneOf specialChars)
  specialChars = "()*|\\?"

parens :: Parseable s => Parsec Void s a -> Parsec Void s a
parens = between (char '(') (char ')')
