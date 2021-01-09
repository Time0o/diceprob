{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

module Diceprob.Integer where

import Diceprob.Bool
import Diceprob.Op

instance Op Int where
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

instance Op [Int] where
  (#=)  s s' = [fromBool (s == s')]
  (#!=) s s' = [fromBool (s /= s')]
  (#<)  s s' = [fromBool (s < s')]
  (#>)  s s' = [fromBool (s > s')]
  (#<=) s s' = [fromBool (s <= s')]
  (#>=) s s' = [fromBool (s >= s')]
