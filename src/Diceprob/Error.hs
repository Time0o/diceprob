{-# LANGUAGE OverloadedStrings #-}

module Diceprob.Error where

import Data.Text (Text, unpack)

import Text.Printf (printf)

err_var = "I cannot find a variable named %s." :: Text

err_loop_var = "A variable must loop over a sequence, but you provided \"%s\"." :: Text

err_cond_var = "Bolean values can only be numbers, but your provided \"%s\".\n\
               \Depending on what you want, you might need to create a function." :: Text

err_seq_range_from = "A sequence range must begin with a number, while you provided: \"%s\"" :: Text

err_seq_range_to = "A sequence range must end with a number, while you provided: \"%s\"" :: Text

err_seq_repeat = "A sequence value frequency or weight must be a number, while you provided \"%s\"." :: Text

err_access = "A position selector must be either a number or a sequence, but you provided \"%s\"" :: Text

errorf :: Text -> Text -> a
errorf msg arg = error $ printf (unpack msg) (unpack arg)
