{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module provides an implementation of type-level natural numbers
    and a `KnownN` that allows us to derive that @c :: `N` -> Constraint@  holds
    for all `N` when we know that @c '`Z`@ and @forall (n :: `N`). c n => c ('`S` n)@.

    See `forallN` for more details.
-}

module Data.N (
  -- Type definition
    N(..)
  , Pred
  , KnownN(..)
  , forallN

  -- * Conversion to/from Nat
  , ToN
  , toN
  , FromN
  , fromN

  -- * Proxy helpers
  , predProxy
  , succProxy
  , unpackPred
  , unpackSucc
  ) where

import Data.Proxy (Proxy(..))
import Data.Typeable hiding (typeOf, TypeRep, typeRep)
import Data.Constraint
-- import GHC.Exts
import GHC.TypeLits
import Data.Bifunctor (Bifunctor(..))
import Control.Monad

-- | Type level natural numbers.
--
-- Why not use `Nat` instead? I don't think `forallN` would be possible.
data N
  = S N
  | Z
  deriving (Eq, Ord, Show, Read, Typeable)

-- | The previous `N` or `Z` if `Z`
type family Pred (n :: N) :: N where
  Pred 'Z = 'Z
  Pred ('S n) = n

-- | Unpack a type-level `N` into either `Z` or @`S` n@ for @n ~ Pred n@.
--
-- This class alone is sufficient to implement many operations, since it allows
-- pattern matching on a type-level natural with resolution of the type to its
-- case when you match on the result.
class KnownN (n :: N) where
  -- | Unpack a type-level `N` into either an equivalence with `Z` or one with
  -- @`S` (`Pred` n)@. Performed recursively, this allows one to unpack a
  -- type-level `N` completely.
  unpackN :: proxy n -> Either (n :~: 'Z) (n :~: 'S (Pred n))

  -- | Any time we can unpack a `N`, we can unpack its predecessor.
  --
  -- (It's easy to see this from within the `KnownN` instances,
  -- the implementations are just `Dict`.)
  unpackPredN :: proxy n -> Dict (KnownN (Pred n))

instance KnownN 'Z where
  unpackN _ = Left Refl
  unpackPredN _ = Dict

instance KnownN n => KnownN ('S n) where
  unpackN _ = Right Refl
  unpackPredN _ = Dict

instance Enum N where
  toEnum n | n < 0 = error "toEnum: Negative N"
  toEnum 0 = Z
  toEnum n = S (toEnum (n - 1))

  fromEnum Z = 0
  fromEnum (S n) = 1 + fromEnum n

  succ = S

instance Num N where
  Z + y = y
  S x + y = x + S y

  Z * _ = Z
  _ * Z = Z
  S x * y = x * y + y

  x - Z = x
  Z - y = error $ "Num (-): negative result: -" ++ show y
  S x - S y = x - y

  abs = id
  signum _ = 1
  negate = error "Num negate: N cannot be negative"

  fromInteger n | n < 0 = error $ "Num fromInteger: N cannot be negative: " ++ show n
  fromInteger 0 = Z
  fromInteger n = S (fromInteger (n - 1))

instance Real N where
  toRational = toRational . toInteger

instance Integral N where
  quotRem x y = join bimap fromInteger $ toInteger x `quotRem` toInteger y

  toInteger Z = 0
  toInteger (S x) = 1 + toInteger x

-- | Using `forallN`, we can convert `KnownN` to any inductive
-- class constraint on `N`.
--
-- Use like so:
--
-- @
--  class ToNatural (n :: `N`) where
--    toNatural_ :: proxy n -> `Natural`
--
--  instance ToNatural '`Z` where
--    toNatural_ _ = 0
--
--  instance ToNatural n => ToNatural ('`S` n) where
--    toNatural_ proxyN = 1 + toNatural_ (`predProxy` proxyN)
--
--  forallToNatural :: `KnownN` n => proxy n -> `Dict` (ToNatural n)
--  forallToNatural _ = `forallN` `Dict` (`Sub` `Dict`)
--
--  toNatural :: `KnownN` n => proxy n -> `Natural`
--  toNatural proxyN = forallToNatural proxyN \``withDict`\` toNatural_ proxyN
-- @
--
-- In other words, this converts `KnownN` to any class constraint that's
-- defined inductively over `N`.
--
-- If you have trouble defining the final method since the @(n :: `N`)@ is
-- only contained in the result, try this method:
--
-- @
--  naryFooProxy :: `KnownN` n => prxy n -> a -> f n a
--  naryFooProxy prxy x = forallFoo prxy \``withDict`\` fooClass x
--
--  naryFoo :: KnownN n => a -> f n a
--  naryFoo = naryFooProxy `Proxy`
-- @
--
forallN ::
     forall (c :: N -> Constraint) (n :: N) (proxy :: N -> *).
     KnownN n
  => Dict (c 'Z)
  -> (forall (n' :: N). c n' :- c ('S n'))
  -> proxy n
  -> Dict (c n)
forallN baseCase inductionCase prxy =
  case unpackN prxy of
    Left Refl -> baseCase
    Right Refl ->
      mapDict inductionCase (forallN baseCase inductionCase (predProxy prxy)) \\
      unpackPred prxy


-- Conversion to/from Nat

-- | Conversion from `Nat` to `N`
type family ToN (n :: Nat) :: N where
  ToN 0 = 'Z
  ToN n = 'S (ToN (n - 1))

-- | Convert a type-level `Nat` to a type-level `N`
toN :: proxy n -> Proxy (ToN n)
toN _ = Proxy

-- | Conversion from `N` to `Nat`
type family FromN (n :: N) :: Nat where
  FromN 'Z = 0
  FromN ('S n) = 1 + FromN n

-- | Convert a type-level `N` to a type-level `Nat`
fromN :: proxy n -> Proxy (FromN n)
fromN _ = Proxy


-- Proxy helpers

-- | The precessor of a type-level `N`
predProxy :: proxy ('S n) -> Proxy n
predProxy _ = Proxy

-- | The the successor of a type-level `N`
succProxy :: proxy n -> Proxy ('S n)
succProxy _ = Proxy

-- | `unpackPred` works for all @n@
unpackPred :: proxy n -> KnownN n :- KnownN (Pred n)
unpackPred prxy = Sub $ unpackPredN prxy `withDict` Dict

-- | `unpackSucc` works for all @n@
unpackSucc :: proxy n -> KnownN n :- KnownN ('S n)
unpackSucc _ = Sub Dict

