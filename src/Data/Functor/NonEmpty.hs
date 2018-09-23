{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module provides a `Functor` transformer that
    is guaranteed to be non-empty by construction.
-}

module Data.Functor.NonEmpty (
  -- * Data types
    NonEmptyT(..)

  -- * Utilities
  , extractNonEmptyT
  , dropNonEmptyT

  -- Conversion
  , toNonEmpty
  , fromNonEmpty
  ) where

import Control.Applicative (Alternative(..))
import Control.Comonad (Comonad(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Distributive (Distributive(..))
import Data.Foldable (toList)
import Data.Function (fix)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Functor.Classes
import Data.Functor.Extend (Extend(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep (Representable(..))
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics ((:*:)(..), Generic, Generic1(..))
import GHC.Read (readPrec)


-- | A `Functor` with a guranteed element, making it non-empty
newtype NonEmptyT f a = NonEmptyT { runNonEmptyT :: (a, f a) } deriving (Generic)

-- | GHC can't derive this, so we do it manually by wrapping and
-- unwrapping the isomorphism:
--
-- @
--  toProduct :: NonEmptyT f a -> (Identity :*: f) a
--
--  fromProduct :: (Identity :*: f) a -> NonEmptyT f a
-- @
--
instance Generic1 f => Generic1 (NonEmptyT f) where
  type Rep1 (NonEmptyT f) = Rep1 (Identity :*: f)

  from1 ~(NonEmptyT (x, xs)) = from1 $ Identity x :*: xs

  to1 = fromProduct . to1
    where
      fromProduct ~(Identity x :*: xs) = NonEmptyT (x, xs)

instance Eq1 f => Eq1 (NonEmptyT f) where
  liftEq eq ~(NonEmptyT (x, xs)) ~(NonEmptyT (y, ys)) =
    eq x y && liftEq eq xs ys

instance (Eq1 f, Eq a) => Eq (NonEmptyT f a) where
  (==) = eq1

instance Ord1 f => Ord1 (NonEmptyT f) where
  liftCompare cmp ~(NonEmptyT (x, xs)) ~(NonEmptyT (y, ys)) =
    cmp x y <> liftCompare cmp xs ys

instance (Ord1 f, Ord a) => Ord (NonEmptyT f a) where
  compare = compare1

instance Show1 f => Show1 (NonEmptyT f) where
  liftShowsPrec sp sl n ~(NonEmptyT (x, xs)) =
    showsBinaryWith sp (liftShowsPrec sp sl) "NonEmptyT" n x xs

instance (Show1 f, Show a) => Show (NonEmptyT f a) where
  showsPrec = showsPrec1

instance Read1 f => Read1 (NonEmptyT f) where
  liftReadPrec rp rl = readBinaryWith rp (liftReadPrec rp rl) "NonEmptyT" (fmap NonEmptyT . (,))

instance (Read1 f, Read a) => Read (NonEmptyT f a) where
  readPrec = readPrec1

deriving instance (Semigroup a, Semigroup (f a)) => Semigroup (NonEmptyT f a)

deriving instance (Monoid a, Monoid (f a)) => Monoid (NonEmptyT f a)

instance Functor f => Functor (NonEmptyT f) where
  fmap f ~(NonEmptyT (x, xs)) = NonEmptyT (f x, fmap f xs)

instance Foldable f => Foldable (NonEmptyT f) where
  foldMap f ~(NonEmptyT (x, xs)) = f x <> foldMap f xs

instance Apply f => Apply (NonEmptyT f) where
  ~(NonEmptyT (f, fs)) <.> ~(NonEmptyT (x, xs)) = NonEmptyT (f x, fs <.> xs)

instance Alternative f => Applicative (NonEmptyT f) where
  pure x = NonEmptyT (x, empty)

  ~(NonEmptyT (f, fs)) <*> ~(NonEmptyT (x, xs)) = NonEmptyT (f x, fs <*> xs)

instance Extend f => Extend (NonEmptyT f) where
  duplicated xs@(~(NonEmptyT (y, ys))) = NonEmptyT (xs, NonEmptyT . (y ,) <$> duplicated ys)

instance Extend f => Comonad (NonEmptyT f) where
  extract = extractNonEmptyT

  duplicate = duplicated

instance (Bind f, Alternative f) => Bind (NonEmptyT f) where
  ~(NonEmptyT (x, xs)) >>- f =
    case f x of
      ~(NonEmptyT (y, ys)) -> NonEmptyT (y, ys <|> (xs >>- fmap dropNonEmptyT f))

instance (Monad f, Alternative f) => Monad (NonEmptyT f) where
  ~(NonEmptyT (x, xs)) >>= f =
    case f x of
      ~(NonEmptyT (y, ys)) -> NonEmptyT (y, ys <|> (xs >>= fmap dropNonEmptyT f))

instance (MonadZip f, Alternative f) => MonadZip (NonEmptyT f) where
  mzipWith f ~(NonEmptyT (x, xs)) ~(NonEmptyT (y, ys)) =
    NonEmptyT (f x y, mzipWith f xs ys)

instance (MonadFix f, Alternative f) => MonadFix (NonEmptyT f) where
  mfix f = case fix (f . extractNonEmptyT) of
             ~(NonEmptyT (x, _)) -> NonEmptyT (x, mfix (dropNonEmptyT . f))

instance Distributive f => Distributive (NonEmptyT f) where
  distribute xs = NonEmptyT (fmap extractNonEmptyT xs, collect dropNonEmptyT xs)

instance Representable f => Representable (NonEmptyT f) where
  type Rep (NonEmptyT f) = Maybe (Rep f)
  tabulate :: (Maybe (Rep f) -> a) -> NonEmptyT f a
  tabulate f = NonEmptyT (f Nothing, tabulate (f . Just))

  index :: NonEmptyT f a -> Maybe (Rep f) -> a
  index ~(NonEmptyT (x, _ ))   Nothing = x
  index ~(NonEmptyT (_, xs)) ~(Just k) = index xs k


-- Utilities

-- | Extract the guraranteed value from a `NonEmptyT`
{-# INLINE extractNonEmptyT #-}
extractNonEmptyT :: NonEmptyT f a -> a
extractNonEmptyT = fst . runNonEmptyT

-- | Extract all but the guaranteed value from a `NonEmptyT`
{-# INLINE dropNonEmptyT #-}
dropNonEmptyT :: NonEmptyT f a -> f a
dropNonEmptyT = snd . runNonEmptyT


-- Conversion

-- | Convert to `NonEmpty`
toNonEmpty :: Foldable f => NonEmptyT f a -> NonEmpty a
toNonEmpty ~(NonEmptyT (x, xs)) = x :| toList xs

-- | Convert from `NonEmpty`
fromNonEmpty :: NonEmpty a -> NonEmptyT [] a
fromNonEmpty ~(x :| xs) = NonEmptyT (x, xs)

