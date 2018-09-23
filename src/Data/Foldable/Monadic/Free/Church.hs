{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-| This module provides an implementation of a free `FoldableM`
    in terms of `FoldM`, using a Church representation.
-}

module Data.Foldable.Monadic.Free.Church (
  -- * Data types
    FoldableFM(..)

  -- * Construction/Destruction
  , head
  , tail
  , uncons
  , cons
  , consM

  -- * Conversion
  , foldableFM
  , toFoldableFM
  , fromCofree
  , toCofreeMaybe
  , fromCofreeMaybe
  , cofreeMaybeT

  -- * Wrapping/unwrapping
  , wrapFoldableFM
  , wrapMaybeT
  , unwrapFoldableFM

  -- * Distributive/Representable
  , distributeFoldM
  , indexFoldM
  , tabulateFoldM

  -- ** MonadFree
  , wrapFoldM
  ) where

import Control.Applicative
import Control.Comonad.Cofree (Cofree(..))
import qualified Control.Foldl as F
import Control.Foldl (FoldM(..), generalize, impurely, purely)
import Control.Foldl.Free (freeFoldM, skipFreeFoldM, toFreeFoldM)
import Control.Foldl.Utils (innerJoinFoldM, stepFoldM)
import Control.Monad (ap)
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Zip
import Data.Distributive (Distributive(..))
import Data.Foldable.Monadic (FoldableM(..), pureFoldMap, toListT)
import Data.Maybe (fromMaybe)
import Data.Profunctor (Profunctor(..))
import Prelude hiding (head, tail)


-- | Church encoding of a free `Foldable` container
newtype FoldableFM m a = FoldableFM
  { runFoldableFM :: forall x y. (x -> a -> m x) -> x -> (x -> m y) -> m y
  }

instance Monad m => Semigroup (FoldableFM m a) where
  xs <> ys = toFoldableFM $ toListT xs <> toListT ys

instance Monad m => Monoid (FoldableFM m a) where
  mempty = FoldableFM $ \_ ini ext -> ext ini

instance (Monad m, Foldable m) => Foldable (FoldableFM m) where
  foldMap f xs =
    foldMap id . (`indexFoldM` xs) . generalize $ F.foldMap f id

instance (Monad m, Traversable m) => Traversable (FoldableFM m) where
  traverse f =
    fmap wrapMaybeT .
    traverse (uncurry $ \x xs -> liftA2 cons (f x) (traverse f xs)) . uncons

instance Monad m => FoldableM m (FoldableFM m) where
  pureFold f FoldableFM {..} =
    (`purely` f) $ \stp ini ext ->
      runFoldableFM (fmap return . stp) ini (return . ext)

  impureFold f FoldableFM {..} =
    (`impurely` f) $ \stp ini ext -> ini >>= \ini' -> runFoldableFM stp ini' ext

instance Functor (FoldableFM m) where
  fmap f FoldableFM{..} = FoldableFM $ \stp ini ext -> runFoldableFM (lmap f . stp) ini ext

instance Monad m => Alternative (FoldableFM m) where
  empty = mempty

  (<|>) = mappend

instance Monad m => Applicative (FoldableFM m) where
  pure x = FoldableFM $ \stp ini ext -> stp ini x >>= ext

  (<*>) = ap

instance Monad m => Monad (FoldableFM m) where
  (>>=) xs = wrapFoldableFM . (`indexFoldM` xs) . tabulateFoldM . pureFoldMap

instance Monad m => MonadPlus (FoldableFM m) where
  mzero = mempty
  mplus = (<|>)

instance Monad m => MonadFail (FoldableFM m) where
  fail _ = empty

instance Monad m => MonadFix (FoldableFM m) where
  mfix f =
    wrapMaybeT $ do
      x <- head . fix $ wrapMaybeT . fmap f . head
      lift $ x `consM` mfix (tail . f)

instance Monad m => MonadZip (FoldableFM m) where
  mzipWith f fx fy =
    wrapMaybeT $ do
      ~(x, xs) <- uncons fx
      ~(y, ys) <- uncons fy
      lift $ f x y `consM` mzipWith f xs ys


-- Construction/Destruction

-- | The first element of a `FoldableFM`, if it exists
head :: Monad m => FoldableFM m a -> MaybeT m a
head xs =
  MaybeT . (`indexFoldM` xs) . generalize $ F.head

-- | All but the first element of a `FoldableFM` if it's non-`null`.
-- Otherwise, returns `empty`
tail :: Monad m => FoldableFM m a -> FoldableFM m a
tail xs =
  wrapFoldableFM . (`indexFoldM` xs) . freeFoldM $
  skipFreeFoldM (toFreeFoldM foldableFM)

-- | Return `Just` the `head` and `tail` or `Nothing` if `null`
uncons :: Monad m => FoldableFM m a -> MaybeT m (a, FoldableFM m a)
uncons xs = (, tail xs) <$> head xs

-- | Prepend an element to a `FoldableFM`
cons :: Monad m => a -> FoldableFM m a -> FoldableFM m a
cons = fmap wrapFoldableFM . consM

-- | Prepend an element to a `FoldableFM` with the monadic effects exposed
-- @consM = mappend . return@
consM :: Monad m => a -> FoldableFM m a -> m (FoldableFM m a)
consM x xs = stepFoldM x foldableFM `indexFoldM` xs


-- Conversion

-- | `FoldM` into a `FoldableFM`
foldableFM :: Monad m => FoldM m a (FoldableFM m a)
foldableFM = FoldM (flip consM) (return mempty) unwrapFoldableFM

-- | Convert any `FoldableM` into a `FoldableFM`.
--
-- @
--  `toListT` . `toFoldableFM` == `toListT`
-- @
--
toFoldableFM :: FoldableM m t => t a -> FoldableFM m a
toFoldableFM xs =
  FoldableFM $ \stp ini ext -> FoldM stp (return ini) ext `impureFold` xs

-- | Sequence the side effects and then collect the values using `consM`
fromCofree :: Monad m => Cofree m a -> FoldableFM m a
fromCofree ~(x :< xs) = wrapFoldableFM $ xs >>= (x `consM`) . fromCofree

-- | Convert to a `Cofree` `MaybeT`
toCofreeMaybe :: Monad m => FoldableFM m a -> MaybeT m (Cofree (MaybeT m) a)
toCofreeMaybe xs = MaybeT $ cofreeMaybeT `indexFoldM` xs

-- | Convert a `Cofree` `MaybeT` to a `FoldableFreeM`
fromCofreeMaybe :: Monad m => Cofree (MaybeT m) a -> FoldableFM m a
fromCofreeMaybe ~(x :< MaybeT xs) =
  wrapFoldableFM $
  xs >>= maybe (return empty) ((x `consM`) . fromCofreeMaybe)

-- | Fold monadically into a `Cofree` `MaybeT`
cofreeMaybeT :: Monad m => FoldM m a (Maybe (Cofree (MaybeT m) a))
cofreeMaybeT = FoldM (\x y -> return . Just $ maybe (y :< empty) ((y :<) . return) x) (return Nothing) return


-- Wrapping/unwrapping

-- | Wrap a layer of monadic context into a `FoldableFM`
wrapFoldableFM :: Monad m => m (FoldableFM m a) -> FoldableFM m a
wrapFoldableFM f =
  FoldableFM $ \stp ini ext ->
    f >>= \FoldableFM {..} -> runFoldableFM stp ini ext

-- | Apply `wrapFoldableFM` and replace `Nothing` with `empty`
wrapMaybeT :: Monad m => MaybeT m (FoldableFM m a) -> FoldableFM m a
wrapMaybeT = wrapFoldableFM . fmap (fromMaybe empty) . runMaybeT

-- | Unwrap a layer of monadic context from a `FoldableFM`
unwrapFoldableFM :: Monad m => FoldableFM m a -> m (FoldableFM m a)
unwrapFoldableFM = indexFoldM foldableFM


-- Distributive/Representable

-- | Uses `tabulateFoldM` and `indexFoldM`. See `Distributive`.
distributeFoldM :: (Monad m, Distributive m, Functor f) => f (FoldM m a b) -> FoldM m a (f b)
distributeFoldM = tabulateFoldM . fmap distribute . distribute . fmap indexFoldM

-- | Index a `FoldM` using a `FoldableFM`. See `Representable`.
indexFoldM :: Monad m => FoldM m a b -> FoldableFM m a -> m b
indexFoldM f FoldableFM {..} =
  (`impurely` f) $ \stp ini ext -> ini >>= \ini' -> runFoldableFM stp ini' ext

-- | Tabulate a `FoldM` by building up successive `FoldableFM`s
tabulateFoldM :: Monad m => (FoldableFM m a -> m b) -> FoldM m a b
tabulateFoldM f = FoldM (flip consM) (return empty) f


-- MonadFree

-- | Wrap a layer of the `Monad`ic context into a `FoldM`. See `MonadFree`
wrapFoldM :: Monad m => m (FoldM m a b) -> FoldM m a b
wrapFoldM = fmap innerJoinFoldM tabulateFoldM . collect indexFoldM

