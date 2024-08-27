{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Text.Regex.Memo.Parser(parseRx) where

import Control.Monad.Combinators.NonEmpty
import Data.Bifunctor
import Data.Functor (($>), (<&>), void)
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Text.Regex.Memo.Rx

type Parseable s = (Stream s, Token s ~ Char)

parseRx :: String -> Either String (Rx 'Parsed)
parseRx str = first errorBundlePretty $ parse (topLevel <* eof) "" str

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
concatenated = concatMany <$> some postModified

postModified :: Parseable s => Parsec Void s (Rx 'Parsed)
postModified = do
  rx <- parens topLevel <|> simpleChar <|> escapedChar
  optional modifier <&> \case
    Nothing -> rx
    Just MStar -> RStar rx
    Just MOpt -> ROptional rx
    Just (MReps from to) -> RReps rx from to
  where
  simpleChar = RCh <$> noneOf specialChars
  escapedChar = RCh <$> (char '\\' *> oneOf specialChars)
  specialChars = "()*|\\?"


data Modifier
  = MStar
  | MOpt
  | MReps Int Int

modifier :: Parseable s => Parsec Void s Modifier
modifier = char '*' $> MStar
       <|> char '?' $> MOpt
       <|> char '{' *> reps
  where
  reps :: Parseable s => Parsec Void s Modifier
  reps = do
    from <- decimal
    mto <- optional $ char ',' *> decimal
    void $ char '}'
    pure $ MReps from (fromMaybe from mto)

parens :: Parseable s => Parsec Void s a -> Parsec Void s a
parens = between (char '(') (char ')')
