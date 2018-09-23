{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}

{-| This module defines a pseudo `Monad`/`Comonad` transformer `ConstT` that
    ignores the provided @* -> *@ type.
-}

module Control.Monad.Trans.Const (
  -- * Data type
    ConstT(..)

  -- * Construction/Destruction
  , constT
  , unConstT
  ) where

import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Monad.Fix
import Data.Coerce
import Data.Functor.Classes
import GHC.Generics
import Text.Read (readPrec)

-- | Ignores the first argument. `ConstT` is neither a `Monad` transformer nor a `Comonad` transformer.
newtype ConstT (m :: * -> *) a = ConstT
  { runConstT :: a
  } deriving (Functor, Foldable, Traversable, Generic, Generic1)

instance Eq1 (ConstT m) where
  liftEq eq x y = unConstT x `eq` unConstT y

instance Eq a => Eq (ConstT m a) where
  (==) = eq1

instance Ord1 (ConstT m) where
  liftCompare cmp x y = unConstT x `cmp` unConstT y

instance Ord a => Ord (ConstT m a) where
  compare = compare1

instance Show1 (ConstT m) where
  liftShowsPrec sp _ n = showsUnaryWith sp "ConstT" n . unConstT

instance Show a => Show (ConstT m a) where
  showsPrec = showsPrec1

instance Read1 (ConstT m) where
  liftReadPrec rp _ = readUnaryWith rp "ConstT" constT

instance Read a => Read (ConstT m a) where
  readPrec = readPrec1

instance Comonad (ConstT m) where
  extract = unConstT
  duplicate = constT

instance ComonadApply (ConstT m)

instance ComonadHoist ConstT where
  cohoist _ = coerce

instance Applicative (ConstT m) where
  pure = constT
  f <*> x = ConstT $ unConstT f $ unConstT x

instance Monad (ConstT m) where
  x >>= f = f $ unConstT x

instance MonadFix (ConstT m) where
  mfix = constT . fix . coerce

-- | `ConstT` implemented in terms of `coerce`
constT :: a -> ConstT m a
{-# INLINE constT #-}
constT = coerce

-- | `runConstT` implemented in terms of `coerce`
unConstT :: ConstT m a -> a
{-# INLINE unConstT #-}
unConstT = coerce

