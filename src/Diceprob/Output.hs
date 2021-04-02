{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Diceprob.Output (
  Output,
  outputsToExport
) where

import Data.Text (Text, intercalate)

import Diceprob.Dice (Dice, pmf, pmfMean, pmfSd, pmfMin, pmfMax)
import Diceprob.Text (textShow)

type Output = (Dice, Maybe Text)

outputToExport :: Int -> Output -> Text
outputToExport i (d, name) = joinLines [outputHeader, "#,%", pmfShowTxt]
  where pmfShow       = map (\(x,p) -> textShow x <> "," <> textShow p)
        pmfStat pmf'  = [pmfMean pmf', pmfSd pmf']
        pmfRange pmf' = [pmfMin pmf', pmfMax pmf']
        pmfShowTxt    = joinLines . pmfShow $ pmf d
        pmfStatTxt    = joinList . map textShow . pmfStat $ pmf d
        pmfRangeTxt   = joinList . map textShow . pmfRange $ pmf d
        outputName    = case name of
                          Nothing    -> "\"output " <> textShow i <> "\""
                          Just name' -> "\"" <> name' <> "\""
        outputHeader  = joinList [outputName, pmfStatTxt, pmfRangeTxt]

outputsToExport :: [Output] -> Text
outputsToExport os = join "\n\n" . map (uncurry outputToExport) $ zip [1..] os

join :: Text -> [Text] -> Text
join = intercalate

joinList :: [Text] -> Text
joinList = join ","

joinLines :: [Text] -> Text
joinLines = join "\n"
