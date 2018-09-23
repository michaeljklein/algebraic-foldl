{-# LANGUAGE DeriveFunctor #-}

{-| This module provides a version of `FoldEM` that only accepts non-empty inputs
-}

module Control.Foldl.Monadic.End.NonEmpty where

import Control.Foldl.Monadic.End (FoldEM(..))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.Profunctor (Profunctor(..))

-- | `FoldEM` with a non-empty input.
--
-- `FoldEM1`'s have a `Category` instance, provided by succeeding early on
-- the given value.
newtype FoldEM1 m a b = FoldEM1
  { runFoldEM1 :: ReaderT a (FoldEM m a) b
  } deriving (Functor)

instance Monad m => Profunctor (FoldEM1 m) where
  lmap f = FoldEM1 . ReaderT . dimap f (lmap f) . runReaderT . runFoldEM1
  rmap = fmap

