{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-| This module defines n-ary functions and operations on them.
-}

module Control.Nary where

import Control.Applicative
import Control.Arrow
import Control.Category (Category(..))
import Control.Comonad (Comonad(..))
import Control.Foldl (Fold(..), FoldM(..))
import Control.Foldl.Utils
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Constraint
import Data.Distributive
import Data.Either
import Data.Functor.Identity
import Data.N
import Data.Profunctor
import Data.Proxy (Proxy(..))
import GHC.Exts
import Prelude hiding ((.), id)

{-
  -- | A fixed arity function returning a `Monad`ic result
  -- newtype AryT n m a b = AryT { runAryT :: Ary n a (m b) }

  -- | An n-ary function
  -- newtype Nary a b = Nary { runNary :: forall n. KnownN n => Ary n a b }

  -- | An n-ary function returning a `Monad`ic result
  -- newtype NaryT m a b = NaryT { runNary :: forall n. KnownN n => AryT n m a b }


  -- instance KnownN n => FoldableM (Reader a) (Ary n a)
  -- instance FoldableM (Reader a) (Nary a)

  -- instance (KnownN n, Monad m) => FoldableM (ReaderT a m) (AryT n m a)
  -- instance Monad m => FoldableM (ReaderT a m) (NaryT m a)


  -- newtype ListN n a = ListN { runListN :: forall x. Ary n a x -> x }

  -- instance KnownN n => Representable (Ary n a) where
  --   type Rep (Ary n a) = ListN n a

  -- instance (KnownN n, Monad m) => RepresentableM m (AryT n m a) where
  --   type RepM (AryT n m a) = ListN n a


  --naryFoldClass :: Nary a b -> Fold a b
  --naryFoldMClass :: Monad m => Nary a (m b) -> FoldM m a b

  --stepAryClass :: a -> Ary ('S n) a b -> Ary n a b
  --stepAryTClass :: a -> AryT ('S n) m a b -> AryT n m a b

  --unwrapAryTClass :: Monad m => AryT n m a b -> m (AryT n m a b)
  --wrapAryTClass :: Monad m => m (AryT n m a b) -> AryT n m a b

  --stepNaryClass :: a -> Nary a b -> Nary a b
  --stepNaryTClass :: Monad m => a -> NaryT m a b -> NaryT m a

  --unwrapNaryT :: NaryT m a b -> m (NaryT m a b)
  --wrapNaryT :: m (NaryT m a b) -> NaryT m a b

  --naryFoldMaybeClass :: Ary n a b -> Fold a (Maybe b)
  --naryFoldMaybeTClass :: Monad m => Ary n a (m b) -> FoldM (MaybeT m) a b

  -- mapNary :: (Ary n a b -> Ary n' c d) -> Nary a b -> Nary c d
  -- mapNaryT :: (AryT m n a b -> AryT m n' c d) -> NaryT m a b -> NaryT m c d

  ----
-}

-- A `Fold` is isomorphic to an n-ary function

-- It's now working!!! But we now need `KnownN` for many operations
--
-- @
--  λ> fromAry ((fromAry . shiftAry . fmap return $ (toAry (\x y z w->x+y+z+w) :: Ary (ToN 4) Int Int)) 1 2 3 :: Ary ('S 'Z) Int Int) 4
--  10
-- @
--


-- | N-ary functions
--
-- @
-- λ> :t Ary0 ()
-- Ary0 () :: Ary 'Z a ()
--
-- λ> :t Ary $ \x -> Ary0 (succ x)
-- Ary $ \x -> Ary0 (succ x) :: Enum b => Ary ('S 'Z) b b
--
-- λ> :t Ary . (Ary .) $ \x y -> Ary0 [x, y]
-- Ary . (Ary .) $ \x y -> Ary0 [x, y] :: Ary ('S ('S 'Z)) b [b]
-- @
--
data Ary (n :: N) (a :: *) (b :: *) where
  Ary0 :: b -> Ary 'Z a b
  Ary :: (a -> Ary n a b) -> Ary ('S n) a b

-- | Unwrap a single layer from a `Ary`
unwrapAry :: Ary ('S n) a b -> a -> Ary n a b
unwrapAry (Ary f) x = f x

-- | Get the @n@ (arity) from a `Ary`
arity :: Ary n a b -> Proxy n
arity _ = Proxy


-- | Conversions between representations of an n-ary function
class KnownN n => AryClass n where
  -- | An @n@-ary function
  type NAry (n :: N) a b :: *

  -- | Convert a specific arity fuinction to a `Ary`
  toAryClass :: NAry n a b -> Ary n a b

  -- | Convert a `Ary` to a specific arity fuinction
  fromAryClass :: Ary n a b -> NAry n a b

instance AryClass 'Z where
  type NAry 'Z a b = b
  toAryClass = Ary0
  fromAryClass (Ary0 x) = x

instance AryClass n => AryClass ('S n) where
  type NAry ('S n) a b = a -> NAry n a b
  toAryClass f = Ary $ \x -> toAryClass (f x)
  fromAryClass (Ary f) = fromAryClass . f

-- | All `KnownN`'s are `AryClass`
forallAryClass :: KnownN n => proxy n -> Dict (AryClass n)
forallAryClass = forallN Dict (Sub Dict)

-- | Convert an n-ary function to `Ary`:
--
-- @
--  toAry ()           :: Ary (ToN 0) Int () :: Ary 'Z Int ()
--  toAry (\_ -> ())   :: Ary (ToN 1) Int () :: Ary ('S 'Z) Int ()
--  toAry (\_ _ -> ()) :: Ary (ToN 2) Int () :: Ary ('S ('S 'Z)) Int ()
-- @
--
toAry :: KnownN n => NAry n a b -> Ary n a b
toAry = toAryProxy Proxy

-- | Helper to fix @n@ in `toAry`
toAryProxy :: KnownN n => proxy n -> NAry n a b -> Ary n a b
toAryProxy prxy f = forallAryClass prxy `withDict` toAryClass f

-- | Convert `Ary` to an n-ary function
fromAry :: KnownN n => Ary n a b -> NAry n a b
fromAry f = forallAryClass (arity f) `withDict` fromAryClass f


-- | Conversions between n-ary functions and `Fold`s
class KnownN n => FoldAry n where
  naryFoldClass :: Ary n a b -> Fold a (Maybe b)
  naryFoldMClass :: Monad m => Ary n a (m b) -> FoldM (MaybeT m) a b
  naryFromFoldClass :: Fold a b -> Ary n a b
  naryFromFoldMClass :: Monad m => FoldM m a b -> Ary n a (m b)

instance FoldAry 'Z where
  naryFoldClass (Ary0 x) = pure $ Just x
  naryFoldMClass (Ary0 x) = pureM $ lift x
  naryFromFoldClass = Ary0 . extract
  naryFromFoldMClass = Ary0 . extractM

instance FoldAry n => FoldAry ('S n) where
  naryFoldClass (Ary f) =
    Fold
      (\x y -> Right $ either ($ y) (stepFold y) x)
      (Left (naryFold . f))
      (either (const empty) extract)
  naryFoldMClass (Ary f) =
    FoldM
      (\x y ->
         either
           (return . Right . ($ y))
           (fmap Right . unwrapFoldM . stepFoldM y)
           x)
      (return (Left (naryFoldM . f)))
      (either (const empty) extractM)
  naryFromFoldClass = Ary . (fmap naryFromFold . flip stepFold)
  naryFromFoldMClass = Ary . (fmap naryFromFoldM . flip stepFoldM)


-- | All `KnownN`'s are `FoldAry`
forallFoldAry :: KnownN n => proxy n -> Dict (FoldAry n)
forallFoldAry = forallN Dict (Sub Dict)

-- | Convert `Ary` to a `Fold`, returning `Nothing` if there are
-- fewer than @n@ elements folded.
naryFold :: KnownN n => Ary n a b -> Fold a (Maybe b)
naryFold f = forallFoldAry (arity f) `withDict` naryFoldClass f

-- | Convert `Ary` to a `FoldM`, returning `empty` if there are
-- fewer than @n@ elements folded.
naryFoldM :: KnownN n => Monad m => Ary n a (m b) -> FoldM (MaybeT m) a b
naryFoldM f = forallFoldAry (arity f) `withDict` naryFoldMClass f

-- | Convert a `Fold` to an arbitrary-sized `Ary` function
naryFromFold :: KnownN n => Fold a b -> Ary n a b
naryFromFold = naryFromFoldProxy Proxy

-- | Helper to fix @n@ in `naryFromFold`
naryFromFoldProxy :: KnownN n => prxy n -> Fold a b -> Ary n a b
naryFromFoldProxy prxy f = forallFoldAry prxy `withDict` naryFromFoldClass f

-- | Convert a `FoldM` to an arbitrary-sized `Ary` function
naryFromFoldM :: (KnownN n, Monad m) => FoldM m a b -> Ary n a (m b)
naryFromFoldM = naryFromFoldMProxy Proxy

-- | Helper to fix @n@ in `naryFromFoldM`
naryFromFoldMProxy :: (KnownN n, Monad m) => proxy n -> FoldM m a b -> Ary n a (m b)
naryFromFoldMProxy prxy f = forallFoldAry prxy `withDict` naryFromFoldMClass f

-- | All `KnownN`'s are `DistributiveAry`
forallDistributiveAry :: KnownN n => proxy n -> Dict (DistributiveAry n)
forallDistributiveAry = forallN Dict (Sub Dict)

instance KnownN n => Distributive (Ary n a) where
  distribute xs =
    forallDistributiveAry (xs `bindProxy` arity) `withDict`
    distributeAryClass xs

  collect f xs =
    forallDistributiveAry (proxyResult f `bindProxy` arity) `withDict`
    collect f xs

-- | Polymorphic definitions of `distribute` and `collect` over `N`
-- |
class KnownN n => DistributiveAry n where
  -- | `distribute` for a particular `N`
  distributeAryClass :: Functor f => f (Ary n a b) -> Ary n a (f b)

  -- | `collect` for a particular `N`
  collectAryClass :: Functor f => (a -> Ary n b c) -> f a -> Ary n b (f c)

instance DistributiveAry 'Z where
  distributeAryClass = Ary0 . fmap fromAry
  collectAryClass f = Ary0 . fmap (fromAry . f)

instance DistributiveAry n => DistributiveAry ('S n) where
  distributeAryClass = Ary . fmap distributeAryClass . collect unwrapAry
  collectAryClass f = Ary . fmap distributeAryClass . collect (unwrapAry . f)

instance (KnownN n, Semigroup b) => Semigroup (Ary n a b) where
  (<>) = liftA2 (<>)

instance (KnownN n, Monoid b) => Monoid (Ary n a b) where
  mempty = return mempty
  mappend = (<>)

instance Profunctor (Ary n) where
  dimap _ g (Ary0 x) = Ary0 $ g x
  dimap f g (Ary h) = Ary $ dimap f (dimap f g) h

  lmap _ (Ary0 x) = Ary0 x
  lmap f (Ary g) = Ary $ dimap f (lmap f) g

  rmap = fmap

instance Strong (Ary ('S 'Z)) where
  first' (Ary f) = Ary $ \(~(x, y)) -> Ary0 (fromAry (f x), y)
  second' (Ary f) = Ary $ \(~(x, y)) -> Ary0 (x, fromAry (f y))

-- | `undefined`
instance Choice (Ary n) where
  right' :: Ary n a b -> Ary n (Either c a) (Either c b)
  right' = undefined -- runKleisliN . (. joinA) . liftAry

instance Functor (Ary n a) where
  fmap f (Ary0 x) = Ary0 $ f x
  fmap f (Ary g) = Ary $ fmap f . g

instance KnownN n => Applicative (Ary n a) where

  pure = pureAryProxy Proxy

  f <*> xs = forallApAry (arity f) `withDict` apAryClass f xs
    where
      forallApAry :: KnownN n => proxy n -> Dict (ApplicativeAry n)
      forallApAry = forallN Dict (Sub Dict)

-- | Polymorphic definition of `pure` and @(`<*>`)@ over `N`
class KnownN n => ApplicativeAry n where
  pureAryClass :: b -> Ary n a b
  apAryClass :: Ary n a (b -> c) -> Ary n a b -> Ary n a c

instance ApplicativeAry 'Z where
  pureAryClass = Ary0
  apAryClass (Ary0 f) (Ary0 x) = Ary0 $ f x

instance ApplicativeAry n => ApplicativeAry ('S n) where
  pureAryClass = Ary . const . pureAryClass
  apAryClass (Ary f) (Ary x) = Ary $ liftM2 apAryClass f x

-- | `pureAry` with a @proxy@ argument to determine the result size.
pureAryProxy :: KnownN n => proxy n -> b -> Ary n a b
pureAryProxy prxy x = forallPureAry prxy `withDict` pureAryClass x
  where
    forallPureAry :: KnownN n => proxy n -> Dict (ApplicativeAry n)
    forallPureAry = forallN Dict (Sub Dict)


instance KnownN n => Monad (Ary n a) where
  xs >>= f = forallBindAry (arity xs) `withDict` bindAryClass xs f
    where
      forallBindAry :: KnownN n => proxy n -> Dict (BindAry n)
      forallBindAry = forallN Dict (Sub Dict)

-- | Polymorphic definition of @(`>>=`)@ over `N`
class KnownN n => BindAry n where
  bindAryClass :: Ary n a b -> (b -> Ary n a c) -> Ary n a c

instance BindAry 'Z where
  bindAryClass (Ary0 x) f = f x

instance BindAry n => BindAry ('S n) where
  bindAryClass (Ary x) f = Ary $ \y -> x y `bindAryClass` flip (unwrapAry . f) y


-- | Polymorphic definition of `extract` and `duplicate` over `N`
class KnownN n => ComonadAry n where
  -- | `extract` for a particular `N`
  extractAryClass :: Monoid a => Ary n a b -> b

  -- | `duplicate` for a particular `N`
  duplicateAryClass :: Monoid a => Ary n a b -> Ary n a (Ary n a b)

instance ComonadAry 'Z where
  extractAryClass (Ary0 x) = x
  duplicateAryClass = Ary0

instance (KnownN n, ComonadAry n) => ComonadAry ('S n) where
  extractAryClass fs@(Ary f) = extractAryClass (f mempty) \\ unpackPred (arity fs)

  duplicateAryClass fs@(Ary f) =
    (Ary $ \x -> shiftAry . Ary $ dimap (mappend x) duplicateAryClass f) \\
    unpackPred (arity fs)


instance (KnownN n, Monoid a) => Comonad (Ary n a) where
  extract xs = forallExtractAry (arity xs) `withDict` extractAryClass xs
    where
      forallExtractAry :: KnownN n => proxy n -> Dict (ComonadAry n)
      forallExtractAry prxy = forallN Dict (Sub $ Dict \\ unpackSucc prxy) prxy

  duplicate xs = forallDuplicateAry (arity xs) `withDict` duplicateAryClass xs
    where
      forallDuplicateAry :: KnownN n => proxy n -> Dict (ComonadAry n)
      forallDuplicateAry = forallN Dict (Sub Dict)


-- | Shift an argument of an `Ary` function in a level
class KnownN n => ShiftAry n where
  shiftAryClass :: Ary ('S n) a (Ary m a b) -> Ary n a (Ary ('S m) a b)

instance ShiftAry 'Z where
  shiftAryClass (Ary f) = Ary0 . Ary $ fromAry . f

instance ShiftAry n => ShiftAry ('S n) where
  shiftAryClass (Ary f) = Ary $ shiftAryClass . f

-- | Shift an argument of an `Ary` function in one level
shiftAry :: KnownN n => Ary ('S n) a (Ary m a b) -> Ary n a (Ary ('S m) a b)
shiftAry x = forallShiftAry (predProxy (arity x)) `withDict` shiftAryClass x
  where
    forallShiftAry :: KnownN n => proxy n -> Dict (ShiftAry n)
    forallShiftAry = forallN Dict (Sub Dict)


-- | Apply an `Ary` function to a value @n@ times
replicateApply :: Ary n a b -> a -> b
replicateApply (Ary0 fx) _ = fx
replicateApply (Ary f) x = replicateApply (f x) x

-- | Apply an `Ary` function to a `Monad`ic value by repeating its action.
replicateMApply :: Monad m => Ary n a b -> m a -> m b
replicateMApply (Ary0 fx) _ = return fx
replicateMApply (Ary f) x = x >>= (`replicateMApply` x) . f




-- | Join two, possibly different, proxies into a single `Proxy`
joinProxy :: forall (proxy :: kp -> *) (proxy' :: ka -> kp) (a :: ka). proxy (proxy' a) -> Proxy a
joinProxy _ = Proxy

-- | @(`>>=`)@ generalized to two (possibly different) proxies
bindProxy :: forall (proxy :: * -> *) (proxy' :: k -> *) (a :: *) (b :: k). proxy a -> (a -> proxy' b) -> Proxy b
bindProxy _ _ = Proxy

-- | Convert a function to a `Proxy` containing its result type
proxyResult :: (a -> b) -> Proxy b
proxyResult _ = Proxy

-- | Arrows that behave like `Kleisli` arrows
class (Monad m, Arrow a) => ArrowM m a | a -> m where
  arrM :: (b -> m c) -> a b c
  joinA :: a (m b) b

-- | The implementations are just:
--
-- @
--  `arrM` = `Kleisli`
--  `joinA` = `Kleisli` `id`
-- @
--
instance Monad m => ArrowM m (Kleisli m) where
  arrM = Kleisli
  joinA = Kleisli id

-- | Implemented using `coerce`
instance ArrowM Identity (->) where
  arrM = coerce `asTypeOf` fmap runIdentity
  joinA = coerce `asTypeOf` runIdentity

