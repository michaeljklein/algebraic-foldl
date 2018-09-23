{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module provides two pseudo-polymorphic data-types:
   `PseudoPoly` and @(`:>-`)@
-}

module Data.PseudoPoly (
  -- * PseudoPoly
    PseudoPoly(..)

  -- ** PseudoPoly helpers
  , fromPseudoPoly
  , eqPseudoPoly
  , swapPseudoPoly
  , transPseudoPoly
  , mapPseudoPoly

  -- * PseudoPoly1
  , (:>-)(..)

  -- ** PseudoPoly1 helpers
  , fromPseudoPoly1
  , eqPseudoPoly1
  , swapPseudoPoly1
  , transPseudoPoly1
  , mapPseudoPoly1
  ) where

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits

-- PseudoPoly


-- | A Pseudo-polymorphic data-type.
--
-- If @`PseudoPoly` a b c@ is inhabited by some terminating value, then the type
-- @b@ is the same as the type @c@.
--
-- To use this equality in practice, pattern-match on the @`PseudoPoly` a b c@
-- to get out the constructor;
-- in the body of the pattern-match, the compiler knows that @b ~ c@.
--
-- Alternatively, you may use the `eqPseudoPoly` helper to extract the
-- prepositional equality: @b `:~:` c@.
--
-- For example, here we can pattern match on `PseudoPoly` to recover the fact
-- that @b ~ c@:
--
-- @
--  foo :: (c -> d) -> `PseudoPoly` a b c -> (b -> d)
--  \f (`PseudoPoly` _) -> f
-- @
--
-- Why is this useful? Take for example `Foldable`, which requires a
-- polymorphic data type, but doesn't strictly have to. This allows us to
-- declare a `Foldable` instance for something like @ByteString@ like so:
--
-- @
--  import qualified Data.ByteString.Char8 as C8
--
--  instance `Foldable` (`PseudoPoly` C8.ByteString Char) where
--    `foldr` f x (`PseudoPoly` xs) = C8.foldr f x xs
-- @
--
-- It looks like magic.
data PseudoPoly a b c where
  PseudoPoly :: a -> PseudoPoly a b b

deriving instance Eq a => Eq (PseudoPoly a b c)
deriving instance Ord a => Ord (PseudoPoly a b c)
deriving instance Show a => Show (PseudoPoly a b c)
deriving instance Read a => Read (PseudoPoly a b b)

instance TypeError ('Text "There is no possible Functor instance for PseudoPoly") =>
         Functor (PseudoPoly a b) where
  fmap = error "unreachable"

instance Foldable (PseudoPoly [a] a) where
  foldr f x (PseudoPoly xs) = foldr f x xs


-- PseudoPoly helpers

-- | Extract the value from a `PseudoPoly`.
-- Since we do not depend on the GADT constraints, we can do so lazily.
fromPseudoPoly :: PseudoPoly a b c -> a
fromPseudoPoly ~(PseudoPoly x) = x

-- | Extract the equality proof from a `PseudoPoly1`.
eqPseudoPoly :: PseudoPoly a b c -> b :~: c
eqPseudoPoly (PseudoPoly _) = Refl

-- | Symmetry of equality
swapPseudoPoly :: PseudoPoly a b c -> PseudoPoly a c b
swapPseudoPoly (PseudoPoly x) = PseudoPoly x

-- | Apply a prepositional type equality to a `PseudoPoly`
transPseudoPoly :: (c :~: d) -> PseudoPoly a b c -> PseudoPoly a b d
transPseudoPoly Refl x = x

-- | Map over the value in a `PseudoPoly`
mapPseudoPoly :: (a -> b) -> PseudoPoly a c d -> PseudoPoly b c d
mapPseudoPoly f (PseudoPoly x) = PseudoPoly (f x)


-- PseudoPoly1

-- | `PseudoPoly` where the second argument is applied to the first
data (:>-) a b c where
  PseudoPoly1 :: a b -> (a :>- b) b

deriving instance Eq (a b) => Eq ((a :>- b) c)
deriving instance Ord (a b) => Ord ((a :>- b) c)
deriving instance Show (a b) => Show ((a :>- b) c)
deriving instance Read (a b) => Read ((a :>- b) b)

instance TypeError ('Text "There is no possible Functor instance for (:>-)") =>
         Functor (a :>- b) where
  fmap = error "unreachable"

-- | A simple example of (`:>-`)
instance Foldable t => Foldable (t :>- a) where
  foldr f x (PseudoPoly1 xs) = foldr f x xs


-- PseudoPoly1 helpers

-- | Extract the value from a @(`:>-`)@.
-- Since we do not depend on the GADT constraints, we can do so lazily.
fromPseudoPoly1 :: (a :>- b) c -> a b
fromPseudoPoly1 ~(PseudoPoly1 x) = x

-- | Extract the equality proof from a @(`:>-`)@.
eqPseudoPoly1 :: (a :>- b) c -> b :~: c
eqPseudoPoly1 (PseudoPoly1 _) = Refl

-- | Symmetry of equality
swapPseudoPoly1 :: (a :>- b) c -> (a :>- c) b
swapPseudoPoly1 (PseudoPoly1 x) = PseudoPoly1 x

-- | Apply a prepositional type equality to a @(`:>-`)@
transPseudoPoly1 :: (c :~: d) -> (a :>- b) c -> (a :>- b) d
transPseudoPoly1 Refl x = x

-- | Map over the value in a @(`:>-`)@
mapPseudoPoly1 :: (a c -> b c) -> (a :>- c) d -> (b :>- c) d
mapPseudoPoly1 f (PseudoPoly1 x) = PseudoPoly1 (f x)

