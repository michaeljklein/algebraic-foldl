{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module provides a version of `FoldM` that only accepts non-empty inputs
-}

module Control.Foldl.Monadic.NonEmpty where

import Control.Foldl
import Control.Comonad.Cofree
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.Profunctor (Profunctor(..))
import Data.List.NonEmpty (NonEmpty(..))
import Control.Foldl.Free

-- | `FoldM` with a non-empty input
newtype FoldM1 m a b = FoldM1
  { runFoldM1 :: ReaderT a (FoldM m a) b
  } deriving (Functor)

instance Monad m => Profunctor (FoldM1 m) where
  lmap f = FoldM1 . ReaderT . dimap f (lmap f) . runReaderT . runFoldM1
  rmap = fmap

-- | Apply a `FoldM1` to a `NonEmpty`
foldM1 :: Monad m => FoldM1 m a b -> NonEmpty a -> m b
foldM1 FoldM1{..} ~(x :| xs) = foldM (runReaderT runFoldM1 x) xs

-- | Convert a `FoldM1` to a `FoldM` that returns `Nothing` if the
-- input is empty.
fromFoldM1 :: Monad m => FoldM1 m a b -> FoldM m a (Maybe b)
fromFoldM1 =
  freeFoldM .
  FreeFoldM .
  return .
  (Nothing :<) .
  ReaderT .
  fmap (runFreeFoldM . fmap Just . toFreeFoldM) . runReaderT . runFoldM1

