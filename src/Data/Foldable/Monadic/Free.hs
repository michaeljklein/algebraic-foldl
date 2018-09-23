{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This module provides an implementation of a free `FoldableM`
    in terms of `FoldM`.
-}

module Data.Foldable.Monadic.Free (
  -- * Data types
    FoldableFreeM(..)

  -- * Construction/Destruction
  , head
  , tail
  , uncons
  , cons
  , consM

  -- * Conversion
  , foldableFreeM
  , toFoldableFreeM
  , fromCofree
  , toCofreeMaybe
  , fromCofreeMaybe
  , cofreeMaybeT

  -- * Wrapping/unwrapping
  , wrapFoldableFreeM
  , wrapMaybeT
  , unwrapFoldableFreeM

  -- * Distributive/Representable
  , distributeFoldM
  , indexFoldM
  , tabulateFoldM

  -- ** MonadFree
  , wrapFoldM
  ) where

import GHC.TypeLits
import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree (Cofree(..))
import Control.Foldl (FoldM(..), generalize)
import qualified Control.Foldl as F
import Control.Foldl.Free (freeFoldM, skipFreeFoldM, toFreeFoldM)
import Control.Foldl.Utils (extractM, innerJoinFoldM, stepFoldM, toFunctionM)
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Free.Class (MonadFree(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Zip
import Data.Distributive (Distributive(..))
import Data.Foldable.Monadic (FoldableM(..), pureFoldMap, toListT)
import Data.Functor.Classes
import Data.Maybe
import Data.Profunctor
import qualified ListT as L (uncons)
import ListT (ListT(..))
import Prelude hiding (head, tail)
import Control.Monad.Rep (RepresentableM(..), bindRepM)


-- | A free `FoldableM` container
newtype FoldableFreeM m a = FoldableFreeM { runFoldableFreeM :: forall x. FoldM m a x -> m x }

instance (Monad m, Eq1 m) => Eq1 (FoldableFreeM m) where
  liftEq eq xs ys = liftEq eq (toListT xs) (toListT ys)

instance Eq1 m => Eq1 (ListT m) where
  liftEq eq xs ys = liftEq (liftEq (liftEq2 eq (liftEq eq)) ) (L.uncons xs) (L.uncons ys)



instance Monad m => Semigroup (FoldableFreeM m a) where
  xs <> ys = toFoldableFreeM $ toListT xs <> toListT ys

instance Monad m => Monoid (FoldableFreeM m a) where
  mempty = FoldableFreeM extractM

instance (Monad m, Foldable m) => Foldable (FoldableFreeM m) where
  foldMap f FoldableFreeM {..} =
    foldMap id . runFoldableFreeM . generalize $ F.foldMap f id

instance (Monad m, Traversable m) => Traversable (FoldableFreeM m) where
  traverse f =
    fmap wrapMaybeT .
    traverse (uncurry $ \x xs -> liftA2 cons (f x) (traverse f xs)) . uncons

instance Monad m => FoldableM m (FoldableFreeM m) where
  pureFold f FoldableFreeM{..} = runFoldableFreeM $ generalize f
  impureFold f FoldableFreeM{..} = runFoldableFreeM f

instance Monad m => Functor (FoldableFreeM m) where
  fmap f FoldableFreeM{..} = FoldableFreeM $ lmap (lmap f) runFoldableFreeM

instance ( Comonad m
         , Monad m
         , TypeError ('Text "This instance returns bottom for: extract (mempty :: FoldableFreeM)," ':$$: 'Text "So it isn't really an instance any more than `Comonad []` could be.")
         ) =>
         Comonad (FoldableFreeM m) where
  extract =
    extract .
    fmap (fromMaybe (error "extracted empty case")) .
    ($ F.generalize F.head) . runFoldableFreeM

  duplicate FoldableFreeM {..} =
    FoldableFreeM $
    (\f x -> f (lmap (\y -> FoldableFreeM $ (\g -> g `toFunctionM` y)) x)) $
    runFoldableFreeM

instance Monad m => Applicative (FoldableFreeM m) where
  pure x = FoldableFreeM $ \fold' -> toFunctionM fold' x

  (<*>) = ap

instance Monad m => Alternative (FoldableFreeM m) where
  empty = mempty

  (<|>) = mappend

instance Monad m => Monad (FoldableFreeM m) where
  (>>=) FoldableFreeM{..} = wrapFoldableFreeM . runFoldableFreeM . tabulateFoldM . pureFoldMap

instance Monad m => MonadFree m (FoldableFreeM m) where
  wrap = wrapFoldableFreeM

instance Monad m => MonadPlus (FoldableFreeM m) where
  mzero = mempty
  mplus = (<|>)

instance Monad m => MonadFail (FoldableFreeM m) where
  fail _ = empty

instance Monad m => MonadFix (FoldableFreeM m) where
  mfix f =
    wrapMaybeT $ do
      x <- head . fix $ wrapMaybeT . fmap f . head
      lift $ x `consM` mfix (tail . f)

instance Monad m => MonadZip (FoldableFreeM m) where
  mzipWith f fx fy =
    wrapMaybeT $ do
      ~(x, xs) <- uncons fx
      ~(y, ys) <- uncons fy
      lift $ f x y `consM` mzipWith f xs ys


-- Construction/Destruction

-- | The first element of a `FoldableFreeM`, if it exists
head :: Monad m => FoldableFreeM m a -> MaybeT m a
head FoldableFreeM {..} =
  MaybeT . runFoldableFreeM . generalize $ F.head

-- | All but the first element of a `FoldableFreeM` if it's non-`null`.
-- Otherwise, returns `empty`
tail :: Monad m => FoldableFreeM m a -> FoldableFreeM m a
tail FoldableFreeM {..} =
  wrapFoldableFreeM . runFoldableFreeM . freeFoldM $
  skipFreeFoldM (toFreeFoldM foldableFreeM)

-- | Return `Just` the `head` and `tail` or `Nothing` if `null`
uncons :: Monad m => FoldableFreeM m a -> MaybeT m (a, FoldableFreeM m a)
uncons xs = (, tail xs) <$> head xs

-- | Prepend an element to a `FoldableFreeM`
cons :: Monad m => a -> FoldableFreeM m a -> FoldableFreeM m a
cons = fmap wrapFoldableFreeM . consM

-- | Prepend an element to a `FoldableFreeM` with the monadic effects exposed
consM :: Monad m => a -> FoldableFreeM m a -> m (FoldableFreeM m a)
consM x FoldableFreeM {..} = runFoldableFreeM $ stepFoldM x foldableFreeM


-- Conversion

-- | `FoldM` into a `FoldableFreeM`
foldableFreeM :: Monad m => FoldM m a (FoldableFreeM m a)
foldableFreeM = FoldM (flip consM) (return mempty) unwrapFoldableFreeM

-- | Convert any `FoldableM` into a `FoldableFreeM`.
--
-- @
--  `toListT` . `toFoldableFreeM` == `toListT`
-- @
--
toFoldableFreeM :: FoldableM m t => t a -> FoldableFreeM m a
toFoldableFreeM xs = FoldableFreeM (`impureFold` xs)

-- | Sequence the side effects and then collect the values using `consM`
fromCofree :: Monad m => Cofree m a -> FoldableFreeM m a
fromCofree ~(x :< xs) = wrapFoldableFreeM $ xs >>= (x `consM`) . fromCofree

-- | Convert to a `Cofree` `MaybeT`
toCofreeMaybe :: Monad m => FoldableFreeM m a -> MaybeT m (Cofree (MaybeT m) a)
toCofreeMaybe FoldableFreeM{..} = MaybeT $ runFoldableFreeM cofreeMaybeT

-- | Convert a `Cofree` `MaybeT` to a `FoldableFreeM`
fromCofreeMaybe :: Monad m => Cofree (MaybeT m) a -> FoldableFreeM m a
fromCofreeMaybe ~(x :< MaybeT xs) =
  wrapFoldableFreeM $
  xs >>= maybe (return empty) ((x `consM`) . fromCofreeMaybe)

-- | Fold monadically into a `Cofree` `MaybeT`
cofreeMaybeT :: Monad m => FoldM m a (Maybe (Cofree (MaybeT m) a))
cofreeMaybeT = FoldM (\x y -> return . Just $ maybe (y :< empty) ((y :<) . return) x) (return Nothing) return


-- Wrapping/unwrapping

-- | Wrap a layer of monadic context into a `FoldableFreeM`
wrapFoldableFreeM :: Monad m => m (FoldableFreeM m a) -> FoldableFreeM m a
wrapFoldableFreeM f = FoldableFreeM $ \fold' -> f >>= indexFoldM fold'

-- | Apply `wrapFoldableFreeM` and replace `Nothing` with `empty`
wrapMaybeT :: Monad m => MaybeT m (FoldableFreeM m a) -> FoldableFreeM m a
wrapMaybeT = wrapFoldableFreeM . fmap (fromMaybe empty) . runMaybeT

-- | Unwrap a layer of monadic context from a `FoldableFreeM`
unwrapFoldableFreeM :: Monad m => FoldableFreeM m a -> m (FoldableFreeM m a)
unwrapFoldableFreeM FoldableFreeM{..} = runFoldableFreeM foldableFreeM


-- Distributive/Representable

-- | Uses `tabulateFoldM` and `indexFoldM`. See `Distributive`.
distributeFoldM :: (Monad m, Distributive m, Functor f) => f (FoldM m a b) -> FoldM m a (f b)
distributeFoldM = tabulateFoldM . fmap distribute . distribute . fmap indexFoldM

-- | Index a `FoldM` using a `FoldableFreeM`. See `Representable`.
indexFoldM :: Monad m => FoldM m a b -> FoldableFreeM m a -> m b
indexFoldM = impureFold

-- | Tabulate a `FoldM` by building up successive `FoldableFreeM`s
tabulateFoldM :: Monad m => (FoldableFreeM m a -> m b) -> FoldM m a b
tabulateFoldM f = FoldM (flip consM) (return empty) f

instance Monad m => Monad (FoldM m a) where
  (>>=) = bindRepM

instance Monad m => RepresentableM m (FoldM m a) where
  type RepM (FoldM m a) = FoldableFreeM m a
  indexM = indexFoldM
  tabulateM = tabulateFoldM

-- MonadFree

-- | Wrap a layer of the `Monad`ic context into a `FoldM`. See `MonadFree`
wrapFoldM :: Monad m => m (FoldM m a b) -> FoldM m a b
wrapFoldM = fmap innerJoinFoldM tabulateFoldM . collect indexFoldM

instance Monad m => MonadFree m (FoldM m a) where
  wrap = wrapFoldM

