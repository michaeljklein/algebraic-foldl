{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-| This module provides a definition of a free `Foldable` container
    in terms of `Fold`, using existential types in a Church encoding.
-}

module Data.Foldable.Free.Church (
  -- * Data types
    FoldableF(..)

  -- * Construction/Destruction
  , head
  , tail
  , uncons
  , cons

  -- * Conversion
  , toFoldableF
  , toFoldableF'
  , foldableF

  -- * Distributive/Representable
  , distributeFold
  , indexFold
  , tabulateFold
  ) where

import Control.Applicative
import Control.Foldl (Fold(..), fold)
import Control.Foldl.Free
  ( freeFold
  , skipFreeFold
  , toFreeFold
  )
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Distributive (Distributive(..))
import Data.Foldable (foldl', toList)
import Data.Functor.Classes
import Data.Maybe (fromMaybe)
import Data.Profunctor (Profunctor(..))
import Prelude hiding (head, tail)

-- | Church encoding of a free `Foldable` container
newtype FoldableF a = FoldableF
  { runFoldableF :: forall x y. (x -> a -> x) -> x -> (x -> y) -> y
  }

instance Functor FoldableF where
  fmap f (FoldableF xs) = FoldableF $ \stp ini ext -> xs (lmap f . stp) ini ext

instance Foldable FoldableF where
  foldl f x (FoldableF xs) = xs f x id
  foldMap f (FoldableF xs) = xs (\x y -> x <> f y) mempty id

instance Eq1 FoldableF where
  liftEq eq xs ys = liftEq eq (toList xs) (toList ys)

instance Eq a => Eq (FoldableF a) where
  (==) = eq1

instance Ord1 FoldableF where
  liftCompare cmp xs ys = liftCompare cmp (toList xs) (toList ys)

instance Ord a => Ord (FoldableF a) where
  compare = compare1

instance Show1 FoldableF where
  liftShowsPrec _ sl n = showsUnaryWith (const sl) "FoldableF" n . toList

instance Show a => Show (FoldableF a) where
  showsPrec = showsPrec1

instance Read1 FoldableF where
  liftReadPrec _ rl = readUnaryWith rl "FoldableF" toFoldableF

instance Read a => Read (FoldableF a) where
  readsPrec = readsPrec1

instance Semigroup (FoldableF a) where
  xs <> ys = toFoldableF $ toList xs ++ toList ys

instance Monoid (FoldableF a) where
  mempty = FoldableF $ \_ ini ext -> ext ini

instance Traversable FoldableF where
  traverse f xs =
    case uncons xs of
      Nothing -> pure empty
      ~(Just (y, ys)) -> liftA2 cons (f y) (traverse f ys)

instance Applicative FoldableF where
  pure x = FoldableF $ \stp ini ext -> ext $ stp ini x

  (<*>) = ap

instance Alternative FoldableF where
  empty = mempty
  (<|>) = (<>)

instance Monad FoldableF where
  xs >>= f = foldMap f xs

instance MonadPlus FoldableF where
  mzero = mempty
  mplus = (<|>)

instance MonadFail FoldableF where
  fail _ = mempty

instance MonadFix FoldableF where
  mfix f = case head (fix (maybe (error "MonadFix FoldableF: empty list") f . head)) of
             Nothing -> empty
             ~(Just x) -> x `cons` mfix (tail . f)

instance MonadZip FoldableF where
  mzipWith f fx fy = fromMaybe empty $ do
    ~(x, xs) <- uncons fx
    ~(y, ys) <- uncons fy
    return $ f x y `cons` mzipWith f xs ys


-- Construction/Destruction

-- | The first element of a `FoldableFree`, if it exists
head :: FoldableF a -> Maybe a
head FoldableF {..} = runFoldableF (\x y -> x <|> return y) Nothing id

-- | All but the first element of a `FoldableFree` if it's non-`null`.
-- Otherwise, returns `empty`
tail :: FoldableF a -> FoldableF a
tail xs = (`indexFold` xs) . freeFold $ skipFreeFold (toFreeFold foldableF)

-- | Return `Just` the `head` and `tail` or `Nothing` if `null`
uncons :: FoldableF a -> Maybe (a, FoldableF a)
uncons xs = (, tail xs) <$> head xs

-- | Prepend an element to a `FoldableFree`
cons :: a -> FoldableF a -> FoldableF a
cons x FoldableF {..} =
  FoldableF $ \stp ini ext -> runFoldableF stp (stp ini x) ext


-- Conversion

-- | Convert any `Foldable` into a `FoldableF`.
--
-- @
--  `toList` . `toFoldableF` == `toList`
-- @
--
toFoldableF :: Foldable t => t a -> FoldableF a
toFoldableF xs = FoldableF $ \stp ini ext -> ext $ foldl stp ini xs

-- | Strict `toFoldableF`
toFoldableF' :: Foldable t => t a -> FoldableF a
toFoldableF' xs = FoldableF $ \stp ini ext -> ext $ foldl' stp ini xs

-- | `Fold` into a `FoldableFree`
foldableF :: Fold a (FoldableF a)
foldableF = Fold (flip cons) mempty id


-- Distributive/Representable

-- | Uses `tabulateFold` and `indexFold`
distributeFold :: Functor f => f (Fold a b) -> Fold a (f b)
distributeFold = tabulateFold . distribute . fmap indexFold

-- | Index a `Fold` using a `FoldableF`
indexFold :: Fold a b -> FoldableF a -> b
indexFold = fold

-- | Tabulate a `Fold` using a `FoldableF`
tabulateFold :: (FoldableF a -> b) -> Fold a b
tabulateFold f = Fold (flip cons) empty f

