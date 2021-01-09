module Diceprob.Text (
  textLength,
  textAt,
  textShow,
  textToInteger
) where

import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read (decimal)

textLength :: Text.Text -> Int
textLength = Text.length

textAt :: Text.Text -> Int -> Char
textAt = Text.index

textShow :: (Show a) => a -> Text.Text
textShow = fromString . show

textToInteger :: Text.Text -> Int
textToInteger t = case Text.Read.decimal t of
  Left e      -> error e
  Right (i,_) -> i
