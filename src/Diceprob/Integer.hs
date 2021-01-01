{-# OPTIONS_GHC -Wall #-}

module Diceprob.Integer where

import Diceprob.Bool
import Diceprob.Op

instance Op Integer where
  (#-)       = negate
  (#+)       = (+)
  (#--)      = (-)
  (#*)       = (*)
  (#/)       = div
  (#^)       = (^)
  (#=)  n n' = fromBool (n == n')
  (#!=) n n' = fromBool (n /= n')
  (#<)  n n' = fromBool (n < n')
  (#>)  n n' = fromBool (n > n')
  (#<=) n n' = fromBool (n <= n')
  (#>=) n n' = fromBool (n >= n')
  (#!)  n    = fromBool (n == 0)
  (#&)  n n' = fromBool (n /= 0 && n' /= 0)
  (#|)  n n' = fromBool (n /= 0 || n' /= 0)

