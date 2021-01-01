{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}

module Diceprob.Op where

class Op a where
  (#-)  :: a -> a
  (#+)  :: a -> a -> a
  (#--) :: a -> a -> a
  (#*)  :: a -> a -> a
  (#/)  :: a -> a -> a
  (#^)  :: a -> a -> a
  (#=)  :: a -> a -> a
  (#!=) :: a -> a -> a
  (#<)  :: a -> a -> a
  (#>)  :: a -> a -> a
  (#<=) :: a -> a -> a
  (#>=) :: a -> a -> a
  (#!)  :: a -> a
  (#&)  :: a -> a -> a
  (#|)  :: a -> a -> a
