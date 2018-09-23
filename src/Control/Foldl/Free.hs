{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module provides a version of `Foldl` in terms of `Cofree` and `ReaderT`
   that's isomorphic to `Foldl`.
-}

module Control.Foldl.Free (
  -- * Data types
    FreeFoldM(..)
  , FreeFold

  -- * Construction and destruction helpers
  , withFreeFoldM
  , withFreeFoldM2
  , asFreeFoldM2

  -- * Expose the underlying Cofree
  , extractFreeFoldM
  , unwrapFreeFold
  , unwrapFreeFoldM
  , wrapFreeFoldM

  -- * Conversion
  , freeFold
  , freeFoldM
  , toFreeFold
  , toFreeFoldM

  -- * Modification
  , stepFreeFold
  , stepFreeFoldM
  , skipFreeFold
  , skipFreeFoldM

  -- * Combinators
  , foldFreeThenFree
  , foldCofreeThenCofree
  , foldFreeThenFreeM
  , foldCofreeThenCofreeM
  , foldFree2
  , foldCofree2
  , unpairCofreeReader
  , unpairFoldFree
  ) where

import Control.Applicative (liftA2)
import Control.Comonad (Comonad(..))
import Control.Comonad.Cofree
import Control.Foldl
import Control.Monad (join, liftM2)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (Reader, ReaderT(..), withReaderT)
import Data.Distributive (Distributive(..))
import Data.Either.Utils (joinEither)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Identity
import Data.Profunctor (Profunctor(..))
import Data.Semigroupoid (Semigroupoid(..))


-- | By expressing the state of a left fold as a `Cofree`:
--
-- @
--  Cofree (current state, new value -> monadic context (new state, new new value -> ..))
-- @
--
-- we can express arbitrary recursions, one step at a time, and allowing things like:
--
-- @
--  If even x then replace all further functions with const 0.
-- @
--
-- This allows us ~(in theory)~ to much more efficiently express combinators such as `foldEThenE`.
--
-- Note: this type is isomorphic to `FoldM`.
newtype FreeFoldM m a b = FreeFoldM { runFreeFoldM :: m (Cofree (ReaderT a m) b) } deriving (Functor)

-- | A pure `FreeFoldM`
type FreeFold = FreeFoldM Identity

-- | I don't think there's a Comonad instance unless we add some pretty strong constraints, like Monoid a, Distributive m..
instance Functor m => Profunctor (FreeFoldM m) where
  lmap f = withFreeFoldM . fmap $ hoistCofree (withReaderT f)
  rmap = fmap

instance Apply m => Apply (FreeFoldM m a) where
  (<.>) = withFreeFoldM2 $
    liftF2
      (\(~(g :< ReaderT gs)) (~(y :< ReaderT ys)) ->
         g y :< ReaderT (liftF2 (asFreeFoldM2 (<.>)) gs ys))

instance Applicative m => Applicative (FreeFoldM m a) where
  pure x = FreeFoldM . pure $ x :< blindReader (runFreeFoldM (pure x))

  (<*>) :: FreeFoldM m a (b -> c) -> FreeFoldM m a b -> FreeFoldM m a c
  (<*>) = withFreeFoldM2 $
    liftA2
      (\(~(g :< ReaderT gs)) (~(y :< ReaderT ys)) ->
         g y :< ReaderT (liftA2 (asFreeFoldM2 (<*>)) gs ys))

-- | This instance uses zipping composition: each result from the first is fed into the second.
--
-- Note: This is _not_ the same type of composition as found in `FoldE`.
instance Applicative m => Semigroupoid (FreeFoldM m) where
  o :: FreeFoldM m b c -> FreeFoldM m a b -> FreeFoldM m a c
  o = withFreeFoldM2 $
    liftA2
      (\(~(x :< ReaderT xs)) (~(y :< ReaderT ys)) ->
         x :< ReaderT ((asFreeFoldM2 o) (xs y) <$> ys))

-- | Helper to abstract-out wrapping and unwrapping a `FreeFoldM`
{-# INLINE withFreeFoldM #-}
withFreeFoldM ::
     (m (Cofree (ReaderT a m) b) -> n (Cofree (ReaderT c n) d))
  -> FreeFoldM m a b
  -> FreeFoldM n c d
withFreeFoldM f = FreeFoldM . f . runFreeFoldM

-- | Helper to abstract-out wrapping and unwrapping a `FreeFoldM`
{-# INLINE withFreeFoldM2 #-}
withFreeFoldM2 ::
     (m1 (Cofree (ReaderT a1 m1) b1) -> m2 (Cofree (ReaderT a2 m2) b2) -> m3 (Cofree (ReaderT a3 m3) b3))
  -> FreeFoldM m1 a1 b1
  -> FreeFoldM m2 a2 b2
  -> FreeFoldM m3 a3 b3
withFreeFoldM2 f ~(FreeFoldM x) ~(FreeFoldM y) = FreeFoldM $ f x y

-- | Helper to abstract-out wrapping and unwrapping a pair of `FreeFoldM`'s
{-# INLINE asFreeFoldM2 #-}
asFreeFoldM2 ::
     (FreeFoldM m1 a1 b1 -> FreeFoldM m2 a2 b2 -> FreeFoldM m3 a3 b3)
  -> m1 (Cofree (ReaderT a1 m1) b1)
  -> m2 (Cofree (ReaderT a2 m2) b2)
  -> m3 (Cofree (ReaderT a3 m3) b3)
asFreeFoldM2 f x y = runFreeFoldM $ f (FreeFoldM x) (FreeFoldM y)

-- | Extract the first result from a `FreeFoldM`
extractFreeFoldM :: Functor m => FreeFoldM m a b -> m b
extractFreeFoldM = fmap extract . runFreeFoldM

-- | Expose a layer of a `FreeFold`
unwrapFreeFold :: FreeFold a b -> Reader a (FreeFold a b)
unwrapFreeFold = unwrapFreeFoldM

-- | Expose a layer of a `FreeFoldM`
{-# SPECIALISE INLINE unwrapFreeFoldM ::
                 FreeFoldM Identity a b ->
                   ReaderT a Identity (FreeFoldM Identity a b)
               #-}
unwrapFreeFoldM :: Monad m => FreeFoldM m a b -> ReaderT a m (FreeFoldM m a b)
unwrapFreeFoldM =
  fmap FreeFoldM . ReaderT . collect (runReaderT . unwrap) . runFreeFoldM

-- | Wrap a `Monad`ic layer around a `FreeFoldM`
{-# INLINE wrapFreeFoldM #-}
wrapFreeFoldM :: Monad m => m (FreeFoldM m a b) -> FreeFoldM m a b
wrapFreeFoldM = FreeFoldM . join . fmap runFreeFoldM

-- Conversion

-- | Construct a `Fold` from a `FreeFold`.
--
-- (This is the the inverse of `toFreeFold`.)
{-# INLINE freeFold #-}
freeFold :: FreeFold a b -> Fold a b
freeFold FreeFoldM {..} =
  Fold (runReaderT . unwrap . extract) runFreeFoldM (extract . extract)

-- | Construct a `FoldM` from a `FreeFoldM`.
--
-- (This is the the inverse of `toFreeFoldM`.)
{-# INLINE freeFoldM #-}
freeFoldM :: Monad m => FreeFoldM m a b -> FoldM m a b
freeFoldM FreeFoldM {..} =
  FoldM (runReaderT . unwrap) runFreeFoldM (return . extract)

-- | Convert a `Fold` to a `FreeFold`.
--
-- (This is the inverse of `freeFold`.)
toFreeFold :: Fold a b -> FreeFold a b
toFreeFold f = (`purely` f) $ \stp ini ext ->
  FreeFoldM $ do
    let step' x = return $ ext x :< ReaderT (step' . stp x)
    step' ini

-- | Convert a `FoldM` to a `FreeFoldM`.
--
-- (This is the inverse of `freeFoldM`.)
toFreeFoldM :: Monad m => FoldM m a b -> FreeFoldM m a b
toFreeFoldM f =
  (`impurely` f) $ \stp ini ext ->
    FreeFoldM $ do
      ini' <- ini
      let step' x = do
            ext' <- ext x
            return $ ext' :< ReaderT ((>>= step') <$> stp x)
      step' ini'

-- Modification

-- | Perform a single step of a `FreeFold`
{-# INLINE stepFreeFold #-}
stepFreeFold :: a -> FreeFold a b -> FreeFold a b
stepFreeFold x ~(FreeFoldM (Identity (_ :< ReaderT xs))) = FreeFoldM $ xs x

-- | Perform a single step of a `FreeFoldM`
{-# INLINE stepFreeFoldM #-}
stepFreeFoldM :: Monad m => a -> FreeFoldM m a b -> FreeFoldM m a b
stepFreeFoldM x FreeFoldM{..} = FreeFoldM $ do
  ~(_ :< ReaderT xs) <- runFreeFoldM
  xs x

-- | Skip the first step of a `FreeFold`
{-# INLINE skipFreeFold #-}
skipFreeFold :: FreeFold a b -> FreeFold a b
skipFreeFold ~(FreeFoldM xs@(Identity (x :< _))) = FreeFoldM . Identity $ x :< ReaderT (const xs)

-- | Skip the first step of a `FreeFoldM`
{-# INLINE skipFreeFoldM #-}
skipFreeFoldM :: Monad m => FreeFoldM m a b -> FreeFoldM m a b
skipFreeFoldM FreeFoldM{..} = FreeFoldM $ do
  xs@(~(x :< _)) <- runFreeFoldM
  return $ x :< return xs

-- | Fold with the first until `Left`,
-- then use its result to construct the second fold and continue.
{-# INLINE foldFreeThenFree #-}
foldFreeThenFree ::
     FreeFoldM (Either b) a b
  -> (b -> FreeFoldM (Either c) a c)
  -> FreeFoldM (Either c) a c
foldFreeThenFree (FreeFoldM f) g =
  FreeFoldM $
  either (runFreeFoldM . g) (`foldCofreeThenCofree` runFreeFoldM . g) f

-- | Fold with the first, extracting each result to provide to the `Cofree`,
-- until `Left`, then use its result to construct the second `Cofree` `ReaderT`
-- and continue.
foldCofreeThenCofree ::
     Cofree (ReaderT a (Either b)) b
  -> (b -> Either c (Cofree (ReaderT a (Either c)) c))
  -> Either c (Cofree (ReaderT a (Either c)) c)
foldCofreeThenCofree ~(x :< ReaderT xs) g =
  return $
  (joinEither . fmap extract . g) x :<
  ReaderT (either g (`foldCofreeThenCofree` g) . xs)

-- | `foldFreeThenFree` generalized to `ExceptT`
{-# INLINE foldFreeThenFreeM #-}
foldFreeThenFreeM ::
     Monad m
  => FreeFoldM (ExceptT b m) a b
  -> (b -> FreeFoldM (ExceptT c m) a c)
  -> FreeFoldM (ExceptT c m) a c
foldFreeThenFreeM (FreeFoldM (ExceptT f)) g =
  FreeFoldM . ExceptT $
  f >>=
  either
    (runExceptT . runFreeFoldM . g)
    (runExceptT . (`foldCofreeThenCofreeM` runFreeFoldM . g))

-- | `foldCofreeThenCofree` generalized to `ExceptT`
foldCofreeThenCofreeM ::
     Monad m
  => Cofree (ReaderT a (ExceptT b m)) b
  -> (b -> ExceptT c m (Cofree (ReaderT a (ExceptT c m)) c))
  -> ExceptT c m (Cofree (ReaderT a (ExceptT c m)) c)
foldCofreeThenCofreeM ~(x :< ReaderT xs) g =
  liftM2
    (:<)
    ((lift . fmap joinEither . runExceptT . fmap extract . g) x)
    (return . ReaderT $
     ExceptT .
     (>>= either (runExceptT . g) (runExceptT . (`foldCofreeThenCofreeM` g))) .
     runExceptT . xs)

-- | Given cases for one or @(previous, current)@ elements
-- and a fold over the results, produce a combined `FreeFoldM`.
foldFree2 ::
     (a -> b)
  -> (a -> a -> b)
  -> FreeFoldM (Either c) b c
  -> FreeFoldM (Either c) a c
foldFree2 f1 f2 = FreeFoldM . fmap (foldCofree2 f1 f2) . runFreeFoldM

-- | Given cases for one or @(previous, current)@ elements
-- and a fold over the results, produce a combined fold.
{-# INLINE foldCofree2 #-}
foldCofree2 ::
     (a -> b)
  -> (a -> a -> b)
  -> Cofree (ReaderT b (Either c)) c
  -> Cofree (ReaderT a (Either c)) c
foldCofree2 f1 f2 ~(x :< ReaderT xs) =
  x :<
  ReaderT
    (\y ->
       (($ y) . runReaderT . unpairCofreeReader) $
       either
         (\z -> z :< blindReader (Left z))
         (hoistCofree (withReaderT (uncurry f2)))
         (xs $ f1 y))

-- | Resolve paired to unpaired inputs by prepending a `ReaderT`
unpairCofreeReader :: Monad m =>
     Cofree (ReaderT (a, a) m) b -> ReaderT a m (Cofree (ReaderT a m) b)
unpairCofreeReader = ReaderT . fmap return . flip loop
  where
    loop y ~(z :< ReaderT zs) = z :< ReaderT (liftM2 fmap loop $ curry zs y)

-- | Given an initial value, we can convert a `FreeFoldM` of sucessive
-- @(previous, current)@ pairs to a simple `FreeFoldM`.
{-# INLINE unpairFoldFree #-}
{-# SPECIALIZE unpairFoldFree :: a -> FreeFoldM (Either b) (a, a) b -> FreeFoldM (Either b) a b #-}
{-# SPECIALIZE unpairFoldFree :: Monad m => a -> FreeFoldM (ExceptT b m) (a, a) b -> FreeFoldM (ExceptT b m) a b #-}
unpairFoldFree :: Monad m => a -> FreeFoldM m (a, a) b -> FreeFoldM m a b
unpairFoldFree x (FreeFoldM y) = FreeFoldM $ y >>= ($ x) . runReaderT . unpairCofreeReader


-- Helpers

-- | Read nothing
{-# INLINE blindReader #-}
blindReader :: m b -> ReaderT a m b
blindReader = ReaderT . const

