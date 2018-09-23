{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This module provides a definition of a free `Foldable` container
    in terms of `Fold`, using existential types.
-}

module Data.Foldable.Free
  (
  -- * Data types
    FoldableFree(..)

  -- * Construction/Destruction
  , head
  , tail
  , uncons
  , cons

  -- * Conversion
  , foldableFree
  , toFoldableFree

  -- * Distributive/Representable
  , distributeFold
  , foldR
  , indexFold
  , tabulateFold
  , divergenceCheck
  ) where

import Control.Applicative
import Control.Comonad (Comonad(..))
import Control.Foldl (Fold(..), fold)
import qualified Control.Foldl as F
import Control.Foldl.Free (freeFold, skipFreeFold, toFreeFold)
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Distributive (Distributive(..))
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep (Representable(..))
import Data.Maybe
import Data.Profunctor
import GHC.Exts (IsList(Item, fromList))
import qualified GHC.Exts as GHC (IsList(toList))
import Prelude hiding (head, tail)

-- | A free `Foldable` container
newtype FoldableFree a = FoldableFree { runFoldableFree :: forall x. Fold a x -> x }

instance IsList (FoldableFree a) where
  type Item (FoldableFree a) = a
  fromList = toFoldableFree
  toList = toList

instance Eq1 FoldableFree where
  liftEq eq xs ys = liftEq eq (toList xs) (toList ys)

instance Eq a => Eq (FoldableFree a) where
  (==) = eq1

instance Ord1 FoldableFree where
  liftCompare cmp xs ys = liftCompare cmp (toList xs) (toList ys)

instance Ord a => Ord (FoldableFree a) where
  compare = compare1

instance Show1 FoldableFree where
  liftShowsPrec _ sl n = showsUnaryWith (const sl) "FoldableFree" n . toList

instance Show a => Show (FoldableFree a) where
  showsPrec = showsPrec1

instance Read1 FoldableFree where
  liftReadPrec _ rl = readUnaryWith rl "FoldableFree" toFoldableFree

instance Read a => Read (FoldableFree a) where
  readsPrec = readsPrec1

instance Semigroup (FoldableFree a) where
  xs <> ys = toFoldableFree $ toList xs ++ toList ys

instance Monoid (FoldableFree a) where
  mempty = FoldableFree extract

instance Functor FoldableFree where
  fmap :: (a -> b) -> FoldableFree a -> FoldableFree b
  fmap f FoldableFree {..} = FoldableFree (lmap (lmap f) runFoldableFree)

instance Foldable FoldableFree where
  foldl f x FoldableFree {..} = runFoldableFree $ Fold f x id
  foldMap f FoldableFree {..} = runFoldableFree $ F.foldMap f id

instance Traversable FoldableFree where
  traverse ::
       Applicative f => (a -> f b) -> FoldableFree a -> f (FoldableFree b)
  traverse f xs =
    case uncons xs of
      Nothing -> pure empty
      ~(Just (y, ys)) -> liftA2 cons (f y) (traverse f ys)

  sequenceA :: Applicative f => FoldableFree (f a) -> f (FoldableFree a)
  sequenceA xs =
    case uncons xs of
      Nothing -> pure empty
      ~(Just (y, ys)) -> liftA2 cons y (sequenceA ys)

instance Applicative FoldableFree where
  pure x = FoldableFree (`fold` Identity x)
  (<*>) = ap

instance Alternative FoldableFree where
  empty = mempty
  (<|>) = mappend

instance Monad FoldableFree where
  FoldableFree {..} >>= f = runFoldableFree $ F.foldMap f id

instance MonadPlus FoldableFree where
  mzero = mempty
  mplus = (<|>)

instance MonadFail FoldableFree where
  fail _ = empty

instance MonadFix FoldableFree where
  mfix :: (a -> FoldableFree a) -> FoldableFree a
  mfix f =
    case head (fix (maybe (error "MonadFix FoldableFree: empty list") f . head)) of
      Nothing -> empty
      ~(Just x) -> x `cons` mfix (tail . f)

instance MonadZip FoldableFree where
  mzipWith ::
       (a -> b -> c) -> FoldableFree a -> FoldableFree b -> FoldableFree c
  mzipWith f fx fy =
    fromMaybe empty $ do
      ~(x, xs) <- uncons fx
      ~(y, ys) <- uncons fy
      return $ f x y `cons` mzipWith f xs ys


-- Construction/Destruction


-- | The first element of a `FoldableFree`, if it exists
head :: FoldableFree a -> Maybe a
head FoldableFree{..} = runFoldableFree F.head

-- | All but the first element of a `FoldableFree` if it's non-`null`.
-- Otherwise, returns `empty`
tail :: FoldableFree a -> FoldableFree a
tail FoldableFree{..} = runFoldableFree . freeFold $ skipFreeFold (toFreeFold foldableFree)

-- | Return `Just` the `head` and `tail` or `Nothing` if `null`
uncons :: FoldableFree a -> Maybe (a, FoldableFree a)
uncons xs = (, tail xs) <$> head xs

-- | Prepend an element to a `FoldableFree`
--
-- Failing implementations:
--
-- @
--  cons x FoldableFree {..} = runFoldableFree $ stepFold x foldableFree
--
--
--  cons' x FoldableFree{..} = runFoldableFree $
--    Fold (flip cons') (return x) id
--
--  λ> cons' () [()]
--  FoldableFree *** Exception: stack overflow
-- @
--
cons :: a -> FoldableFree a -> FoldableFree a
cons = mappend . pure

-- Conversion

-- | `Fold` into a `FoldableFree`
foldableFree :: Fold a (FoldableFree a)
foldableFree = Fold (flip cons) mempty id

-- | Convert any `Foldable` into a `FoldableFree`.
--
-- @
--  `toList` . `toFoldableFree` == `toList`
-- @
--
toFoldableFree :: Foldable t => t a -> FoldableFree a
toFoldableFree xs = FoldableFree (`fold` xs)


-- Distributive/Representable

-- | Uses `tabulateFold` and `indexFold`
distributeFold :: Functor f => f (Fold a b) -> Fold a (f b)
distributeFold = tabulateFold . distribute . fmap indexFold

instance Distributive (Fold a) where
  distribute = distributeFold

-- | Apply a strict left 'Fold' to a 'Foldable' container,
-- right-to-left
foldR :: Foldable f => Fold a b -> f a -> b
foldR (Fold step begin done) as = foldl cons_ done as begin
  where
    cons_ k a x = k $! step x a

-- | We build `tabulate` by folding over the input values and
-- feeding them to the `FoldableFree`.
--
-- @
--  `index` == `foldR`
--
--  instance Representable (Fold a) where
--    type Rep (Fold a) = FoldableFree a
-- @
indexFold :: Fold a b -> FoldableFree a -> b
indexFold = foldR

-- | Tabulate a `Fold` by building up successive `FoldableFree`s.
--
-- Quick benchmark:
--
-- @
--  λ> F.fold ((tabulateFold . indexFold) (F.lastN 3)) ([1..2^12] :: [Int])
--  [4094,4095,4096]
--  (1.67 secs, 1,952,032,256 bytes)
--
--  λ> F.fold (F.lastN 3) ([1..2^12] :: [Int])
--  [4094,4095,4096]
--  (0.01 secs, 1,632,784 bytes)
-- @
--
tabulateFold :: (FoldableFree a -> b) -> Fold a b
tabulateFold f = Fold (flip cons) empty f

instance Representable (Fold a) where
  type Rep (Fold a) = FoldableFree a
  index = indexFold
  tabulate = tabulateFold

-- | If this returns anything other than:
--
-- @
--  `Just` "It didn't diverge!"
-- @
--
-- Then we have a bug.
--
-- The bug it's designed to find is:
-- either `tabulateFold` or `indexFold` is non-productive until reaching the
-- end of the input. In this case, the input is unlimited while the `Fold`
-- can return upon reaching the first element.
--
-- And.. it's not returning..
--
-- @
--  λ> divergenceCheck
--  ^CInterrupted.
-- @
--
-- Well we have a bug..
--
-- Now I'm wondering whether that's a feature.. of the strict implementaton of `fold`..
--
-- It's a feature, regular folds also diverge:
--
-- @
--  λ> F.fold F.head [1..]
--  ^CInterrupted.
-- @
--
-- Even using `generalizeMaybe` diverges:
--
-- @
--  λ> F.foldM (generalizeMaybe F.head) [1..] :: MaybeT Identity Int
--  MaybeT (Identity ^CInterrupted.
-- @
--
divergenceCheck :: Maybe String
divergenceCheck =
  "It should have diverged!" <$ fold ((tabulateFold . indexFold) F.head) ([0 ..] :: [Int])

