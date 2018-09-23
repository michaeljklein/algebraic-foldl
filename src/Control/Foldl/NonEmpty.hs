{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| This module provides a version of `Fold` that only accepts non-empty inputs
-}

module Control.Foldl.NonEmpty (
  -- * Data type declarations
    Fold1(..)

  -- * Folding
  , fold1
  , fold1With
  , fold1NonEmpty

  -- * Conversion
  , fromFold1
  , fromFold1With
  , toFold1

  -- * Folds
  , head1
  , last1
  ) where

import Control.Comonad (Comonad(..))
import Control.Foldl
import Control.Foldl.Utils (stepFold)
import Control.Monad (liftM2)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Profunctor (Profunctor(..))
import Prelude hiding (last)


-- | `Fold` with a guaranteed non-empty input
newtype Fold1 a b = Fold1
  { runFold1 :: ReaderT a (Fold a) b
  } deriving (Functor, Applicative)

instance Profunctor Fold1 where
  lmap f = Fold1 . ReaderT . dimap f (lmap f) . runReaderT . runFold1
  rmap = fmap

-- Folding

-- | Apply a `Fold1` to a possibly empty `Foldable`
fold1 :: Foldable t => Fold1 a b -> t a -> Maybe b
fold1 = fold . fromFold1

-- | `fold1` with provided initial input
fold1With :: Foldable t => Fold1 a b -> a -> t a -> b
fold1With = fmap fold . fromFold1With

-- | Apply a `Fold1` to a `NonEmpty`
fold1NonEmpty :: Fold1 a b -> NonEmpty a -> b
fold1NonEmpty f ~(x :| xs) = fold1With f x xs

-- Conversion

-- | Convert a `Fold1` to a `Fold` by returning `Nothing` when there's no input
fromFold1 :: Fold1 a b -> Fold a (Maybe b)
fromFold1 (Fold1 (ReaderT f)) =
  Fold (\x y -> Just . liftM2 maybe f stepFold y $ x) Nothing (fmap extract)

-- | `fromFold1` with provided initial input
fromFold1With :: Fold1 a b -> a -> Fold a b
fromFold1With = runReaderT . runFold1

-- | Convert a `Fold` to a `Fold1` by passing the `ReaderT`-provided argument
-- to `stepFold` and using it to update the `Fold`
toFold1 :: Fold a b -> Fold1 a b
toFold1 = Fold1 . ReaderT . flip stepFold


-- Folds

-- | The first element of a non-empty collection
{-# INLINE head1 #-}
head1 :: Fold1 a a
head1 = Fold1 $ ReaderT pure

-- | The last element of a non-empty collection
{-# INLINE last1 #-}
last1 :: Fold1 a a
last1 = Fold1 . ReaderT $ \x -> fromMaybe x <$> last

