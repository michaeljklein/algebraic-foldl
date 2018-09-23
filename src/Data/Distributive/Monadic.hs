{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module defines `Distributive` for a fixed `Monad`
    instead of an arbitrary `Functor`.
-}

module Data.Distributive.Monadic (
  -- * Type class
    DistributiveM(..)

  -- ** distributem and collectm in terms of each other
  , distributemByCollectm
  , collectmByDistributem

  -- *  Utilities defined using `DistributiveM`
  , wrapFoldM

  -- ** ConstT
  , ConstT(..)

  -- *  MonadTrans instances
  , distributemTrans
  , collectmTrans

  -- *  Generics
  , gdistributem
  , gcollectm
  , GDistributiveM(..)
  ) where

import Control.Comonad (Comonad(..))
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad.Trans.Cofree (CofreeT(..))
import qualified Control.Comonad.Trans.Cofree as Trans (CofreeF(..))
import Control.Comonad.Trans.Coiter (CoiterT(..))
import Control.Comonad.Trans.Env (EnvT(..), ask, lowerEnvT)
import Control.Comonad.Trans.Store (StoreT(..), pos)
import Control.Comonad.Trans.Traced (TracedT(..))
import Control.Foldl (FoldM(..))
import Control.Foldl.Utils (extractM, innerJoinFoldM, stepFoldM, unwrapFoldM)
import Control.Monad (join, liftM2)
import Control.Monad.Free (Free(..))
import Control.Monad.Free.Church (F(..), fromF, toF)
import Control.Monad.Trans.Accum (AccumT(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Const
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Free (FreeT(..))
import qualified Control.Monad.Trans.Free as Trans (FreeF(..))
import Control.Monad.Trans.Free.Church (FT(..), fromFT, toFT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Iter (IterT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.RWS (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Complex (Complex)
import Data.Distributive (Distributive(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Dual, Product, Sum)
import GHC.Generics
  ( (:*:)(..)
  , (:.:)(..)
  , Generic1(..)
  , M1(..)
  , Par1(..)
  , Rec1(..)
  , U1(..)
  )

-- | This is the categorical dual of `Traversable`, for a `Monad` fixed by
-- our choice of @f@.
--
-- If you define one of `distributem`, `collectm` you may define the other
-- using `collectmByDistributem`, `distributemByCollectm`, respectively.
--
-- All `MonadTrans`formers that result in a `Monad` are instances of
-- `DistributiveM`, see `collectmTrans`.
class Monad m => DistributiveM m f | f -> m where
  -- | The dual of `sequence`, for a fixed `Monad`
  --
  -- @
  --  `distributem` = `collectm` `id`
  -- @
  --
  distributem :: m (f a) -> f (m a)
  default distributem :: (Generic1 f, GDistributiveM m (Rep1 f)) => m (f a) -> f (m a)
  distributem = gdistributem

  -- | Map then `distributem`:
  --
  -- @
  --  `collectm` f = `distributem` . `fmap` f
  -- @
  --
  collectm :: (a -> f b) -> m a -> f (m b)
  default collectm :: (Generic1 f, GDistributiveM m (Rep1 f)) => (a -> f b) -> m a -> f (m b)
  collectm = gcollectm

instance Monad m => DistributiveM m (FoldM m a) where
  distributem f = FoldM (fmap unwrapFoldM . flip stepFoldM) f (return . extractM)

  collectm = collectmByDistributem

-- | If there were no functional dependencies, this would be the "other"
-- instance for `Identity`:
--
-- @
--  instance `Monad` m => `DistributiveM` m `Identity` where
--    `distributem` :: m (`Identity` a) -> `Identity` (m a)
--    `distributem` = `Identity` . `fmap` `runIdentity`
-- @
--
-- But it'd violate the functional dependency since `Identity` does not
-- determine @m@.
--
-- By ignoring the chosen `Monad`, we can recreate the instance.
--
-- See `wrappedCofreeFToProd` for an example.
instance Monad m => DistributiveM m (ConstT m) where
  distributem = ConstT . fmap runConstT
  collectm f = ConstT . fmap (runConstT . f)


-- distributem and collectm in terms of each other

-- | `distributem` in terms of `collectm`
distributemByCollectm :: DistributiveM m f => m (f a) -> f (m a)
distributemByCollectm = collectm id

-- | `collectm` in terms of `distributem`
collectmByDistributem :: DistributiveM m f => (a -> f b) -> m a -> f (m b)
collectmByDistributem f = distributem . fmap f

-- Utilities defined using `DistributiveM`

-- | Implement `wrap` using `distributem` and `innerJoinFoldM`
wrapFoldM :: Monad m => m (FoldM m a b) -> FoldM m a b
wrapFoldM = innerJoinFoldM . distributem


-- GHC.Generics instances

instance (DistributiveM m f, DistributiveM m g) =>
         DistributiveM m (f :*: g)

instance (DistributiveM m f, DistributiveM m g, Functor f) =>
         DistributiveM m (f :.: g)

instance (DistributiveM m f, DistributiveM m g, Functor f) =>
         DistributiveM m (Compose f g)

instance DistributiveM Identity Par1

instance DistributiveM m f => DistributiveM m (M1 i c f)

instance DistributiveM m f => DistributiveM m (Rec1 f)

instance DistributiveM Identity U1


-- Lifted instances

instance DistributiveM Identity Identity

instance DistributiveM Identity Complex

instance DistributiveM Identity Dual

instance DistributiveM Identity Sum

instance DistributiveM Identity Product

instance DistributiveM Identity Proxy


-- MonadTrans instances

instance Monad m => DistributiveM m (ReaderT a m) where
  distributem = distributemTrans -- ReaderT . distribute . fmap runReaderT
  collectm = collectmTrans -- f xs = ReaderT $ \x -> flip (runReaderT . f) x <$> xs

instance (Monoid w, Monad m) => DistributiveM m (AccumT w m) where
  distributem = distributemTrans -- AccumT . fmap (>>= fmap (first return)) . collect runAccumT
  collectm = collectmTrans -- f xs = AccumT $ \x -> xs >>= fmap (first return) . flip (runAccumT . f) x

instance Monad m => DistributiveM m (ContT r m) where
  distributem = distributemTrans -- xs = ContT $ \f -> xs >>= ($ f . return) . runContT
  collectm = collectmTrans -- f xs = ContT $ \g -> xs >>= \y -> (runContT . f) y (g . return)

instance Monad m => DistributiveM m (ExceptT e m) where
  distributem = distributemTrans -- ExceptT . (>>= fmap (fmap return) . runExceptT)
  collectm = collectmTrans -- f xs = ExceptT $ xs >>= fmap (fmap return) . runExceptT . f

instance Monad m => DistributiveM m (IdentityT m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance Monad m => DistributiveM m (MaybeT m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance (Monoid w, Monad m) => DistributiveM m (RWST r w s m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance (Monoid w, Monad m) => DistributiveM m (Strict.RWST r w s m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance Monad m => DistributiveM m (StateT s m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance Monad m => DistributiveM m (Strict.StateT s m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance (Monoid w, Monad m) => DistributiveM m (WriterT w m) where
  distributem = distributemTrans
  collectm = collectmTrans

instance (Monoid w, Monad m) => DistributiveM m (Strict.WriterT w m) where
  distributem = distributemTrans
  collectm = collectmTrans


-- | `distributem` for any `MonadTrans` that's a `Monad` when its base functor is.
distributemTrans :: (Monad m, MonadTrans t, Monad (t m)) => m (t m a) -> t m (m a)
distributemTrans = fmap return . join . lift

-- | `collectm` for any `MonadTrans` that's a `Monad` when its base functor is.
--
-- We `lift` the `m` then @(`>>=`)@ the function to the result.
-- Finally, we `return` inside of the transformer.
collectmTrans :: (Monad m, MonadTrans t, Monad (t m)) => (a -> t m b) -> m a -> t m (m b)
collectmTrans f = fmap return . (>>= f) . lift


-- Comonad transformers

instance (Comonad m, Monad m) => DistributiveM m (EnvT e m) where
  distributem = liftM2 EnvT (ask . extract) (fmap lowerEnvT)
  collectm = collectmByDistributem

instance (Comonad m, Monad m) => DistributiveM m (StoreT s m) where
  distributem = liftM2 StoreT (fmap (\(~(StoreT acc _)) -> distribute acc)) (pos . extract)
  collectm = collectmByDistributem

instance (Monoid w, Monad m) => DistributiveM m (TracedT w m) where
  distributem = TracedT . fmap (distribute . runTracedT)
  collectm f = TracedT . (>>= runTracedT . fmap return . f)


-- Free

instance (Comonad m, Monad m) => DistributiveM m (Cofree m) where
  distributem xs =
    fmap extract xs :< (xs >>= (\(~(_ :< ys)) -> fmap return <$> ys))
  collectm = collectmByDistributem

-- | A wrapped representation of `CofreeF` without the outermost
-- `Comonad`ic layer
newtype WrappedCofreeF f w a = WrappedCofreeF
  { runWrappedCofreeF :: Trans.CofreeF f a (CofreeT f w a)
  }

instance (Functor f, Functor w) => Functor (WrappedCofreeF f w) where
  fmap f = WrappedCofreeF . bimap f (fmap f) . runWrappedCofreeF

-- | Convert `CofreeT` to our wrapped representation
toWrappedCofreeF :: Functor w => CofreeT f w a -> w (WrappedCofreeF f w a)
toWrappedCofreeF = fmap WrappedCofreeF . runCofreeT

-- | Convert `WrappedCofreeF` to a product, which has a suitable automatically
-- derived `DistributiveM` instance.
{-# INLINE wrappedCofreeFToProd #-}
wrappedCofreeFToProd ::
     WrappedCofreeF f w a -> (ConstT f :*: Compose (IdentityT f) (CofreeT f w)) a
wrappedCofreeFToProd ~(WrappedCofreeF (x Trans.:< xs)) =
  ConstT x :*: Compose (IdentityT xs)

-- | Convert `WrappedCofreeF` from a product
{-# INLINE wrappedCofreeFFromProd #-}
wrappedCofreeFFromProd ::
     (ConstT f :*: Compose (IdentityT f) (CofreeT f w)) a -> WrappedCofreeF f w a
wrappedCofreeFFromProd ~(ConstT x :*: Compose (IdentityT xs)) =
  WrappedCofreeF $ x Trans.:< xs

instance (DistributiveM m f, Functor f) =>
         DistributiveM m (WrappedCofreeF m f) where
  distributem = wrappedCofreeFFromProd . collectm wrappedCofreeFToProd
  collectm f = wrappedCofreeFFromProd . collectm (wrappedCofreeFToProd . f)

instance (DistributiveM m f, Functor f) => DistributiveM m (CofreeT m f) where
  distributem =
    CofreeT .
    fmap runWrappedCofreeF . getCompose . collectm (Compose . toWrappedCofreeF)
  collectm f =
    CofreeT .
    fmap runWrappedCofreeF .
    getCompose . collectm (Compose . toWrappedCofreeF . f)

instance Monad m => DistributiveM m (CoiterT m) where
  distributem = CoiterT . (>>= fmap (bimap return (fmap return)) . runCoiterT)
  collectm f =
    CoiterT . (>>= fmap (bimap return (fmap return)) . runCoiterT . f)

-- | Deconstruct a `Free`
{-# INLINE free #-}
free :: (a -> b) -> (f (Free f a) -> b) -> Free f a -> b
free f _ (Pure x) = f x
free _ g ~(Free x) = g x

instance Monad m => DistributiveM m (Free m) where
  distributem = Free . fmap (fmap return)
  collectm f =
    Free . (>>= free (return . return . return) (fmap return <$>) . f)

instance Monad m => DistributiveM m (F m) where
  distributem = toF . collectm fromF
  collectm = collectmByDistributem

-- | A wrapped representation of `FreeF` without the outermose `Monad`ic layer
newtype WrappedFreeF f m a = WrappedFreeF
  { runWrappedFreeF :: Trans.FreeF f a (FreeT f m a)
  }

instance (Functor f, Monad m) => Functor (WrappedFreeF f m) where
  fmap f = WrappedFreeF . bimap f (fmap f) . runWrappedFreeF

-- | Convert `FreeT` to our wrapped representation
toWrappedFreeF :: Functor m => FreeT f m a -> m (WrappedFreeF f m a)
toWrappedFreeF = fmap WrappedFreeF . runFreeT

-- | Deconstruct a `FreeF`
{-# INLINE freeF #-}
freeF :: (a -> c) -> (f b -> c) -> Trans.FreeF f a b -> c
freeF f _ (Trans.Pure x) = f x
freeF _ g ~(Trans.Free x) = g x

instance (Monad m, Monad n) => DistributiveM m (WrappedFreeF m n) where
  distributem =
    WrappedFreeF .
    Trans.Free .
    (>>= freeF (return . return . return) (fmap return <$>) . runWrappedFreeF)
  collectm f =
    WrappedFreeF .
    Trans.Free .
    (>>= freeF (return . return . return) (fmap return <$>) .
         runWrappedFreeF . f)

instance (DistributiveM m n, Monad n) => DistributiveM m (FreeT m n) where
  distributem =
    FreeT .
    fmap runWrappedFreeF . getCompose . collectm (Compose . toWrappedFreeF)
  collectm f =
    FreeT .
    fmap runWrappedFreeF . getCompose . collectm (Compose . toWrappedFreeF . f)

instance (DistributiveM m n, Monad n) => DistributiveM m (FT m n) where
  distributem = toFT . collectm fromFT
  collectm = collectmByDistributem

instance Monad m => DistributiveM m (IterT m) where
  distributem = IterT . (>>= fmap (bimap return (fmap return)) . runIterT)
  collectm f = IterT . (>>= fmap (bimap return (fmap return)) . runIterT . f)


-- Generics

-- | A default implementation of `distributem` for `Generic1` types
gdistributem :: (GDistributiveM m (Rep1 f), Generic1 f) => m (f a) -> f (m a)
gdistributem = to1 . gcollectm' from1

-- | A default implementation of `collectm` for `Generic1` types
gcollectm :: (GDistributiveM m (Rep1 f), Generic1 f) => (a -> f b) -> m a -> f (m b)
gcollectm f = to1 . gcollectm' (from1 . f)


-- | The typeclass implementing `gdistributem` (as `gdistributem'`)
-- for `Generic1` types
class Monad m => GDistributiveM (m :: * -> *) (f :: * -> *) | f -> m where
  gdistributem' :: m (f a) -> f (m a)
  gdistributem' = gcollectm' id

  gcollectm' :: (a -> f b) -> m a -> f (m b)

instance GDistributiveM m f => GDistributiveM m (M1 i c f) where
  gcollectm' f = M1 . gcollectm' (unM1 . f)

instance GDistributiveM Identity U1 where
  gcollectm' _ _ = U1

instance GDistributiveM Identity Par1 where
  gcollectm' f = Par1 . fmap (unPar1 . f)

instance (GDistributiveM m f, GDistributiveM m g) =>
         GDistributiveM m (f :*: g) where
  gcollectm' f xs = gcollectm' leftF xs :*: gcollectm' rightF xs
    where
      leftF = (\(~(ys :*: _)) -> ys) . f
      rightF = (\(~(_ :*: zs)) -> zs) . f

instance DistributiveM m f => GDistributiveM m (Rec1 f) where
  gcollectm' f xs = Rec1 $ collectm (unRec1 . f) xs

instance (DistributiveM m f, GDistributiveM m g, Functor f) =>
         GDistributiveM m (f :.: g) where
  gcollectm' f xs = Comp1 $ gdistributem' <$> collectm (unComp1 . f) xs

