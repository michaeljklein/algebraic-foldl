{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveFunctor #-}

{-| This module provides a class for types you can fold over
    with a `Monad`ic result.
-}

module Data.Foldable.Monadic where

import Control.Applicative (WrappedMonad(..))
import Control.Comonad (Comonad(..))
import Control.Comonad.Cofree (Cofree(..))
import qualified Control.Comonad.Trans.Cofree as Trans
  ( CofreeF(..)
  , CofreeT(..)
  )
import Control.Comonad.Trans.Coiter (CoiterT(..))
import Control.Foldl
  ( Fold(..)
  , FoldM(..)
  , fold
  , foldM
  , foldMap
  , list
  , sink
  )
import Control.Foldl.Utils
  ( extractM
  , stepFold
  , stepFoldM
  , toFunction
  , toFunctionM
  , unwrapFoldM
  )
import Control.Monad (join)
import Control.Monad.Free (Free(..))
import qualified Control.Monad.Free.Ap as Ap (Free(..))
import Control.Monad.Free.Church (F(..))
import Control.Monad.Trans.Accum (AccumT(..), runAccumT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.Free as Trans (FreeF(..), FreeT(..))
import Control.Monad.Trans.Free.Church (FT(..), fromFT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Iter (IterT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.RWS.Lazy (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Lazy (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.Writer.Lazy (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import Data.Functor.Extend (Extend(..))
import Data.Functor.NonEmpty (NonEmptyT(..))
import Data.Profunctor
import ListT (ListT(..), fromFoldable)
import qualified ListT as L
import Prelude hiding (foldMap)

-- -- | `VectorM` instances!
-- class Monad m => FoldableM m (t :: * -> *) | t -> m where
--   foldM :: Monoid a => t a -> m a
--   foldrM :: (a -> b -> m b) -> b -> t a -> m b
--   foldrM' :: (a -> b -> m b) -> b -> t a -> m b
--   foldlM :: (b -> a -> m b) -> b -> t a -> m b
--   foldlM' :: (b -> a -> m b) -> b -> t a -> m b
--   foldr1M :: (a -> a -> m a) -> t a -> m a
--   foldl1M :: (a -> a -> m a) -> t a -> m a
--   toListT :: t a -> ListT m a
--   nullM :: t a -> m Bool
--   lengthM :: t a -> m Int
--   elemM :: Eq a => a -> t a -> m Bool
--   maximumM :: Ord a => t a -> m a
--   minimumM :: Ord a => t a -> m a
--   sumM :: Num a => t a -> m a
--   productM :: Num a => t a -> m a
--   {-# MINIMAL foldMapM | foldrM #-}

-- | A relatively minimal implementation
class Monad m => FoldableM m (t :: * -> *) | t -> m where
  pureFold :: Fold a b -> t a -> m b
  impureFold :: FoldM m a b -> t a -> m b

instance Monad m => FoldableM m (ListT m) where
  pureFold f = impureFold (lmap NonEmptyT $ innerFold f) . MaybeT . L.uncons
  impureFold f = impureFold (lmap NonEmptyT $ innerFoldM f) . MaybeT . L.uncons

-- | Treats `Nothing` as an empty container.
-- Use `WrappedMonad` to preserve it.
instance Monad m => FoldableM m (MaybeT m) where
  pureFold f = fmap (fold f) . runMaybeT
  impureFold f = (>>= foldM f) . runMaybeT

-- | Treats exceptions as empty containers.
-- Use `WrappedMonad` to preserve them.
instance Monad m => FoldableM m (ExceptT e m) where
  pureFold f = fmap (fold f) . runExceptT
  impureFold f = (>>= foldM f) . runExceptT

-- | Drops the final accumulated value. Use `WrappedMonad` to preserve it.
instance (Monad m, Monoid w) => FoldableM m (AccumT w m) where
  pureFold f = fmap (toFunction f . fst) . extract . runAccumT
  impureFold f = (>>= toFunctionM f . fst) . extract . runAccumT

instance Monad m => FoldableM m (WrappedMonad m) where
  pureFold f = fmap (toFunction f) . unwrapMonad
  impureFold f = (>>= toFunctionM f) . unwrapMonad

instance Monad m => FoldableM m (IdentityT m) where
  pureFold f = fmap (toFunction f) . runIdentityT
  impureFold f = (>>= toFunctionM f) . runIdentityT

-- | Drops the final state and writer value.
-- Use `WrappedMonad` to preserve them.
instance (Monad m, Monoid r, Monoid s) => FoldableM m (RWST r w s m) where
  pureFold f = fmap (toFunction f . fst3) . extract . extract . runRWST
  impureFold f = (>>= toFunctionM f . fst3) . extract . extract . runRWST

-- | Drops the final state and writer value.
-- Use `WrappedMonad` to preserve them.
instance (Monad m, Monoid r, Monoid s) => FoldableM m (Strict.RWST r w s m) where
  pureFold f = fmap (toFunction f . fst3) . extract . extract . Strict.runRWST
  impureFold f = (>>= toFunctionM f . fst3) . extract . extract . Strict.runRWST

instance (Monad m, Monoid r) => FoldableM m (ReaderT r m) where
  pureFold f = fmap (toFunction f) . extract . runReaderT
  impureFold f = (>>= toFunctionM f) . extract . runReaderT

-- | Drops the final state. Use `WrappedMonad` to preserve it.
instance (Monad m, Monoid s) => FoldableM m (StateT s m) where
  pureFold f = fmap (toFunction f . fst) . extract . runStateT
  impureFold f = (>>= toFunctionM f . fst) . extract . runStateT

-- | Drops the final state. Use `WrappedMonad` to preserve it.
instance (Monad m, Monoid s) => FoldableM m (Strict.StateT s m) where
  pureFold f = fmap (toFunction f . fst) . extract . Strict.runStateT
  impureFold f = (>>= toFunctionM f . fst) . extract . Strict.runStateT

-- | Drops the writer value. Use `WrappedMonad` to preserve it.
instance Monad m => FoldableM m (WriterT w m) where
  pureFold f = fmap (toFunction f . fst) . runWriterT
  impureFold f = (>>= toFunctionM f . fst) . runWriterT

-- | Drops the writer value. Use `WrappedMonad` to preserve it.
instance Monad m => FoldableM m (Strict.WriterT w m) where
  pureFold f = fmap (toFunction f . fst) . Strict.runWriterT
  impureFold f = (>>= toFunctionM f . fst) . Strict.runWriterT

---- | We need `MonadPlus`, otherwise we get the following:
----
---- @
----   `pureFold` f (x `:<` xs) = xs `>>=` `impureFold` (x \``stepFold`\` f)
---- @
----
---- If no monadic action fails, it'll simply squash all of the monadic actions forever (likely while building up a nice large memory-leak).
---- If any fail, the failing monadic effect will be returned without doing any folding.
--instance MonadPlus m => FoldableM m (Cofree m) where
--  pureFold f (x :< xs) = (xs >>= pureFold (x `stepFold` f)) `mplus` return (toFunction f x)
--  impureFold f (x :< xs) = (`mplus` toFunctionM f x) $ do
--    y  <- unwrapFoldM $ stepFoldM x f
--    ys <- xs
--    impureFold y ys

instance FoldableM m f => FoldableM m (Cofree f) where
  pureFold f ~(x :< xs) = impureFold (innerFold (stepFold x f)) xs
  impureFold f ~(x :< xs) = impureFold (innerFoldM (stepFoldM x f)) xs

instance FoldableM m f => FoldableM m (Trans.CofreeT f m) where
  pureFold f (Trans.CofreeT xs) = xs >>= \(~(y Trans.:< ys)) -> impureFold (innerFold (stepFold y f)) ys
  impureFold f (Trans.CofreeT xs) = xs >>= \(~(y Trans.:< ys)) -> impureFold (innerFoldM (stepFoldM y f)) ys


instance Monad m => FoldableM m (CoiterT m) where
  pureFold f (CoiterT xs) = xs >>= \(~(y, ys)) -> pureFold (stepFold y f) ys
  impureFold f (CoiterT xs) =
    xs >>= \(~(y, ys)) -> unwrapFoldM (stepFoldM y f) >>= (`impureFold` ys)

instance Monad m => FoldableM m (Free m) where
  pureFold f  (Pure x) = return $ toFunction f x
  pureFold f ~(Free x) = x >>= pureFold f

  impureFold f  (Pure x) = toFunctionM f x
  impureFold f ~(Free x) = x >>= impureFold f

instance Monad m => FoldableM m (Ap.Free m) where
  pureFold f  (Ap.Pure x) = return $ toFunction f x
  pureFold f ~(Ap.Free x) = x >>= pureFold f

  impureFold f  (Ap.Pure x) = toFunctionM f x
  impureFold f ~(Ap.Free x) = x >>= impureFold f

instance Monad m => FoldableM m (F m) where
  pureFold f (F x) = x (return . toFunction f) join

  impureFold f (F x) = x (toFunctionM f) join

instance FoldableM m f => FoldableM m (Trans.FreeT f m) where
  pureFold f (Trans.FreeT xs) = xs >>= freeF (return . toFunction f) (impureFold (innerFold f))
  impureFold f (Trans.FreeT xs) = xs >>= freeF (toFunctionM f) (impureFold (innerFoldM f))

instance (FoldableM m f, Functor f) => FoldableM m (FT f m) where
  pureFold f = pureFold f . fromFT
  impureFold f = impureFold f . fromFT

instance Monad m => FoldableM m (IterT m) where
  pureFold f (IterT x) = x >>= either (return . toFunction f) (pureFold f)
  impureFold f (IterT x) = x >>= either (toFunctionM f) (impureFold f)

instance FoldableM m f => FoldableM m (NonEmptyT f) where
  pureFold f ~(NonEmptyT (x, xs)) = pureFold (stepFold x f) xs

  impureFold f ~(NonEmptyT (x, xs)) = do
    f' <- unwrapFoldM $ stepFoldM x f
    impureFold f' xs




-- | Convert to `ListT`
toListT :: FoldableM m t => t a -> ListT m a
toListT = join . lift . pureFold listT

-- | Fold into a `ListT` (migrate to Utils)
listT :: Monad m => Fold a (ListT m a)
listT = fromFoldable <$> list

-- | `foldMap` for a `FoldableM`
pureFoldMap :: (FoldableM m t, Monoid w) => (a -> w) -> t a -> m w
pureFoldMap f = pureFold $ foldMap f id

-- | `pureFoldMap`, where the function returns a monadic result
impureFoldMap :: (FoldableM m t, Monoid w) => (a -> m w) -> t a -> m w
impureFoldMap f = impureFold $ sink f

-- | Apply all of the elements in a `FoldableM` to a `Fold`,
-- retaining the partial result.
stepsFold :: FoldableM m t => Fold a b -> t a -> m (Fold a b)
stepsFold = pureFold . duplicate

-- | Apply all of the elements in a `FoldableM` to a `FoldM`,
-- retaining the partial result.
stepsFoldM :: FoldableM m t => FoldM m a b -> t a -> m (FoldM m a b)
stepsFoldM = impureFold . duplicated

-- | Fold over the inner argument using the `FoldableM` instance
innerFold :: FoldableM m t => Fold a b -> FoldM m (t a) b
innerFold f = FoldM stepsFold (return f) (return . extract)

-- | Fold over the inner argument using the `FoldableM` instance
innerFoldM :: FoldableM m t => FoldM m a b -> FoldM m (t a) b
innerFoldM f = FoldM stepsFoldM (unwrapFoldM f) extractM

-- Helpers

-- | Deconstruct a `Trans.FreeF` like `either`
{-# INLINE freeF #-}
freeF :: (a -> c) -> (f b -> c) -> Trans.FreeF f a b -> c
freeF f _ (Trans.Pure x) = f x
freeF _ g (Trans.Free x) = g x

-- | The first of three
{-# INLINE fst3 #-}
fst3 :: (a, b, c) -> a
fst3 ~(x, _, _) = x

-- | TODO:
--
-- @
--  instance (FoldableM m f, FoldableM m g) => FoldableM m (f :.: g)
--  instance (FoldableM m f, FoldableM m g) => FoldableM m (f :*: g)
--  instance (FoldableM m f, FoldableM m g) => FoldableM m (f :+: g)
--
--  newtype WrappedFoldableT t m a = WrappedFoldableT { runWrappedFoldableT :: m (t a) }
--  instance (Monad m, Foldable t) => FoldableM m (WrappedFoldable t m)
--
--  instance (PrimMonad m, MVector v a) => FoldableM m (v (PrimState m) :>- a) where
--    pureFold = pureFold . mstream
--
--  instance Monad m => FoldableM m (Stream m) where
--    _ -- This is where FoldableM shines :D
-- @
--
todo :: ()
todo = ()

