{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{-| This module provides a version of `FoldE` for strictly non-empty inputs
-}

module Control.Foldl.End.NonEmpty where

import Control.Category (Category(..))
import Control.Foldl.End (FoldE(..), foldEToFree, freeToFoldE)
import Control.Foldl.Free
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Profunctor (Profunctor(..))
import Prelude hiding ((.), id)

-- | `FoldE1`'s have a `Category` instance, provided by succeeding early on
-- the given value.
newtype FoldE1 a b = FoldE1
  { runFoldE1 :: ReaderT a (FoldE a) b
  } deriving (Functor, Applicative, Monad)

instance Profunctor FoldE1 where
  lmap f = FoldE1 . ReaderT . dimap f (lmap f) . runReaderT . runFoldE1
  rmap = fmap

-- | The `id` implementation is straightforward:
--
-- @
--  id = FoldE1 $ ReaderT return
-- @
--
instance Category FoldE1 where
  id :: FoldE1 a a
  id = FoldE1 $ ReaderT return
  (.) :: FoldE1 b c -> FoldE1 a b -> FoldE1 a c
  f . FoldE1 (ReaderT g) = FoldE1 . ReaderT $ appendFoldE1 f . g

-- | Convert a `FoldE1` to an unwrapped `FreeFoldM`
foldE1ToFreeFold :: FoldE1 a b -> ReaderT a (Either b) (FreeFoldM (Either b) a b)
foldE1ToFreeFold = ReaderT . fmap (Right . foldEToFree) . runReaderT . runFoldE1

-- | Convert an unwrapped `FreeFoldM` to a `FoldE1`
freeFoldToFoldE1 :: ReaderT a (Either b) (FreeFoldM (Either b) a b) -> FoldE1 a b
freeFoldToFoldE1 = FoldE1 . ReaderT . fmap (freeToFoldE . wrapFreeFoldM) . runReaderT

-- | `o`, but the first argument is a `FoldE1`
appendFoldE1 :: FoldE1 b c -> FoldE a b -> FoldE a c
appendFoldE1 f g = freeToFoldE $ appendFreeE1 (foldE1ToFreeFold f) (foldEToFree g)

-- | `undefined`
appendFreeE1 :: ReaderT b (Either c) (FreeFoldM (Either c) b c) -> FreeFoldM (Either b) a b -> FreeFoldM (Either c) a c
appendFreeE1 ~(ReaderT f) ~(FreeFoldM g) = undefined f g

-- | `undefined`
appendFreeE :: FreeFoldM (Either c) b c -> FreeFoldM (Either b) a b -> FreeFoldM (Either c) a c
appendFreeE = undefined

