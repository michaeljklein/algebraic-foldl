{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module defines a version of `Representable` where the function
    tabulated/indexed returns a monadic result.

    In other words, instead of providing an isomorphism with `Reader`,
    we provide one with `ReaderT` for a fixed `Monad`.
-}

module Control.Monad.Rep (
  -- * Type class
    RepresentableM(..)

  -- * Functor
  , fmapRepM

  -- * Distributive
  , distributeRepM
  , collectRepM

  -- * DistributiveM
  , distributemRepM
  , collectmRepM

  -- * Apply/Applicative
  , apRepM
  , pureRepM
  , liftRM2
  , liftRM3

  -- * Bind/Monad
  , bindRepM

  -- * MonadFix
  , mfixRepM

  -- * MonadZip
  , mzipWithRepM
  , mzipRepM

  -- * MonadReader
  , fromReaderT
  , toReaderT
  , askRepM
  , asksRepM
  , mapRepM
  , withRepM
  , localRepM

  -- * Extend
  , duplicatedRepM
  , extendedRepM

  -- * Comonad
  , duplicateRepM
  , extendRepM
  , extractRepM

  -- * Comonad, with user-specified monoid
  , duplicateRepMBy
  , extendRepMBy
  , extractRepMBy

  -- * WithIndex
  , imapRepM
  , imapMRepM
  , ifoldMapRepM
  , itraverseRepM

  -- * MonadFree
  , wrapRepM

  -- * Lifting other operations
  , liftRepM
  , liftCallCC
  , liftCatch

  -- * Generics
  , gtabulateM
  , gindexM

  , GRepM
  , GRepM'
  , GTabulateM(..)
  , GIndexM(..)
  , WrappedRepM(..)

  , todo
  ) where

import Control.Comonad (Comonad(..))
import Control.Monad (liftM2, liftM3)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Bits (Bits, FiniteBits)
import Data.Complex (Complex)
import Data.Data (Data)
import Data.Distributive (Distributive(..))
import Data.Distributive.Monadic
import Data.Foldable (fold)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Ix (Ix)
import Data.Profunctor.Unsafe (Profunctor(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Dual, Product, Semigroup(..), Sum)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Foreign.Storable (Storable)
import GHC.Generics
  ( (:*:)(..)
  , (:.:)(..)
  , Generic
  , Generic1(..)
  , M1(..)
  , Par1(..)
  , Rec1(..)
  , U1(..)
  )
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Text.Printf (PrintfArg)

-- There are a number of "boring", i.e. trivial, instances:
-- Monad m => RepresentableM m (WrappedMonad m) where
--   type RepM (WrappedMonad m) = ()
--   tabulateM f = WrappedMonad (f ())
--   indexM = const
-- ..

-- | A `Functor` @f@ is `RepresentableM` for a given `Monad` @m@ if `tabulateM`
-- and `indexM` witness an isomorphism to @`ReaderT` (`RepM` f) m@.
--
-- @
--  `tabulateM` . `indexM` == `id`
--  `indexM` . `tabulateM` == `id`
--  `tabulateM` . `return` . `return` == `return`
-- @
--
-- Every `RepresentableM` `Functor` whose base `Monad` is a right adjoint
-- is also a right adjoint.
--
-- Inuitively, a `RepresentableM` `Functor` is a `Representable` `Functor` on
-- the Kleisli category of the base `Monad`.
--
-- I'd expect some superclass requirement, but it ends up looking like just
-- @`Applicative` f@:
--
-- @
-- class `Applicative` f => DistributiveA f where
--   distributeM :: `Traversable` t => t (f a) -> f (t a)
--
-- instance `Applicative` m => DistributiveM (`ReaderT` a m) where
--   distributeM = `ReaderT` . `fmap` `sequenceA` . `distribute` . `fmap` `runReaderT`
-- @
--
class DistributiveM m f => RepresentableM m f | f -> m where
  -- | If no definition is provided, this will default to `GRepM`.
  type RepM f :: *
  type RepM f = GRepM f

  -- |
  -- @
  --  `fmap` (`fmap` f) . `tabulateM` == `tabulateM` . `fmap` f
  -- @
  --
  -- If no definition is provided, this will default to `gtabulateM`.
  tabulateM :: (RepM f -> m a) -> f a
  default tabulateM :: (Generic1 f, GRepM f ~ RepM f, GTabulateM m (Rep1 f)) => (RepM f -> m a) -> f a
  tabulateM = gtabulateM

  -- | If no definition is provided, this will default to 'gindexM'.
  indexM :: f a -> (RepM f -> m a)
  default indexM :: (Generic1 f, GRepM f ~ RepM f, GIndexM m (Rep1 f)) => f a -> (RepM f -> m a)
  indexM = gindexM


-- GHC.Generics instances

instance (RepresentableM m f, RepresentableM m g) =>
         RepresentableM m (f :*: g)

instance (RepresentableM m f, RepresentableM m g, Monad (f :.: g), Functor f) =>
         RepresentableM m (f :.: g)

instance (RepresentableM m f, RepresentableM m g, Monad (Compose f g), Functor f) =>
         RepresentableM m (Compose f g)

instance RepresentableM Identity Par1

instance RepresentableM m f => RepresentableM m (M1 i c f)

instance RepresentableM m f => RepresentableM m (Rec1 f)

instance RepresentableM Identity U1


-- Lifted instances

instance RepresentableM Identity Identity

instance RepresentableM Identity Complex

instance RepresentableM Identity Dual

instance RepresentableM Identity Sum

instance RepresentableM Identity Product

instance RepresentableM Identity Proxy


-- Interesting instances

instance Monad m => RepresentableM m (ReaderT a m) where
  type RepM (ReaderT a m) = a
  tabulateM = ReaderT
  indexM = runReaderT

-- instance Monad m => RepresentableM m (FoldM m a) where
--   type RepM (FoldM m a) = FoldableFree a
--   tabulateM = fromFunctionM
--   indexM = foldM

-- -- | Hmm, instead of `fromFunctionEM`, shouldn't we be able
-- -- to encode a function like `lineE` using it?
-- instance RepresentableM m (FoldEM m a) where
--   type RepM (FoldEM m a) = FoldableFree a
--   tabulateM = fromFunctionEM
--   indexM = foldEM


--   type RepM (CofreeM m) = Free Maybe (RepM f)
--   indexM (_ :< xs) (Pure k) = indexM xs k
--   indexM (x :< _ ) (Free Nothing) = return x
--   indexM (_ :< xs) (Free (Just k)) = xs >>= (`indexM` k)

--   tabulateM f = f Nothing :< _ f

-- Functor

-- | `fmap` inside the `Monad`ic result of a `RepresentableM`
fmapRepM :: RepresentableM m f => (a -> b) -> f a -> f b
fmapRepM f = tabulateM . fmap (fmap f) . indexM


-- Distributive

-- | `collect` after `indexM`ing
distributeRepM :: (RepresentableM m f, Distributive m, Functor w) => w (f a) -> f (w a)
distributeRepM wf = tabulateM $ \k -> collect (`indexM` k) wf

-- | `collect` after `indexM`ing the result of the provided function
collectRepM :: (RepresentableM m f, Distributive m, Functor w) => (a -> f b) -> w a -> f (w b)
collectRepM f w = tabulateM $ \k -> collect ((`indexM` k) . f) w


-- DistributiveM

-- | `distributem` for `RepresentableM`
distributemRepM :: RepresentableM m f => m (f a) -> f (m a)
distributemRepM = tabulateM . collect indexM

-- | `collect` for `RepresentableM`
collectmRepM :: RepresentableM m f => (a -> f b) -> m a -> f (m b)
collectmRepM f = tabulateM . collect indexM . fmap f


-- Apply/Applicative

-- | Uses the isomorphism
apRepM :: RepresentableM m f => f (a -> b) -> f a -> f b
apRepM f g = tabulateM $ liftM2 (<*>) (indexM f) (indexM g)

-- | `tabulateM` ignoring the index and `return`ing the value
pureRepM :: RepresentableM m f => a -> f a
pureRepM = tabulateM . const . return

-- | Uses the isomorphism
liftRM2 :: RepresentableM m f => (a -> b -> c) -> f a -> f b -> f c
liftRM2 f fa fb = tabulateM $ \k -> liftM2 f (indexM fa k) (indexM fb k)

-- | Uses the isomorphism
liftRM3 :: RepresentableM m f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftRM3 f fa fb fc = tabulateM $ \k -> liftM3 f (indexM fa k) (indexM fb k) (indexM fc k)


-- Bind/Monad

-- | Equivalent to:
--
-- @
--  \\m f -> `runReaderT` $ `toReaderT` m `>>=` `toReaderT` . f
-- @
--
bindRepM :: RepresentableM m f => f a -> (a -> f b) -> f b
bindRepM m f = tabulateM $ \k -> f <$> indexM m k >>= (`indexM` k)


-- MonadFix

-- | Equivalent to:
--
-- @
--  `runReaderT` . `mfix` . `fmap` `toReaderT`
-- @
--
mfixRepM :: (RepresentableM m f, MonadFix m) => (a -> f a) -> f a
mfixRepM = fromReaderT . mfix . fmap toReaderT


-- MonadZip

-- | `indexM`, `mzipWith`, then `tabulateM`
mzipWithRepM :: (RepresentableM m f, MonadZip m) => (a -> b -> c) -> f a -> f b -> f c
mzipWithRepM f as bs = tabulateM $ \k -> mzipWith f (indexM as k) (indexM bs k)

-- | `indexM`, `mzip`, then `tabulateM`
mzipRepM :: (RepresentableM m f, MonadZip m) => f a -> f b -> f (a, b)
mzipRepM as bs = tabulateM $ \k -> mzip (indexM as k) (indexM bs k)


-- MonadReader

-- | @`RepresentableM` m f@ is isomorphic to @`ReaderT` (`RepM` f) m a@:
--
-- @
--  `fromReaderT` . `toReaderT` == `id`
--  `toReaderT` . `fromReaderT` == `id`
-- @
--
fromReaderT :: RepresentableM m f => ReaderT (RepM f) m a -> f a
fromReaderT = tabulateM . runReaderT

-- | The other half of the isomorphism, see: `fromReaderT`
toReaderT :: RepresentableM m f => f a -> ReaderT (RepM f) m a
toReaderT = ReaderT . indexM

-- | Fetch the value of the environment.
--
-- (`ask` specialized to `RepresentableM`)
askRepM :: RepresentableM m f => f (RepM f)
askRepM = tabulateM return

-- | Retrieve a function of the current environment.
--
-- (`asks` specialized to `RepresentableM`)
asksRepM :: RepresentableM m f
         => (RepM f -> a)  -- ^ The selector function to apply to the environment.
         -> f a
asksRepM = tabulateM . fmap return

-- | Transform the computation inside a `RepresentableM`.
--
-- (`mapReaderT` specialized to `RepresentableM`)
mapRepM :: RepresentableM m f => (m a -> m b) -> f a -> f b
mapRepM f xs = tabulateM $ f . indexM xs

-- | Execute a computation in a modified environment.
--
-- (`withReaderT` specialized to `RepresentableM`)
withRepM :: RepresentableM m f
         => (RepM f -> RepM f)  -- ^ The function to modify the environment.
         -> f a                 -- ^ Computation to run in the modified environment.
         -> f a
withRepM f m = tabulateM $ indexM m . f

-- | Execute a computation in a modified environment.
--
-- (`local` specialized to `RepresentableM`)
localRepM :: RepresentableM m f
          => (RepM f -> RepM f) -- ^ The function to modify the environment.
          -> f a                -- ^ Computation to run in the modified environment.
          -> f a
localRepM = withRepM


-- Extend

-- | @`duplicateRepMBy` (`<>`)@
duplicatedRepM :: (RepresentableM m f, Semigroup (RepM f)) => f a -> f (f a)
duplicatedRepM = duplicateRepMBy (<>)

-- | @`extendRepMBy` (`<>`)@
extendedRepM :: (RepresentableM m f, Semigroup (RepM f)) => (f a -> b) -> f a -> f b
extendedRepM = extendRepMBy (<>)


-- Comonad

-- | @`duplicateRepMBy` `mappend`@
duplicateRepM :: (RepresentableM m f, Monoid (RepM f)) => f a -> f (f a)
duplicateRepM = duplicateRepMBy mappend

-- | @`extendRepMBy` `mappend`@
extendRepM :: (RepresentableM m f, Monoid (RepM f)) => (f a -> b) -> f a -> f b
extendRepM = extendRepMBy mappend

-- | @`extractRepMBy` `mempty`@
extractRepM :: (RepresentableM m f, Monoid (RepM f), Comonad m) => f a -> a
extractRepM = extractRepMBy mempty


-- Comonad, with user-specified monoid

-- | `indexM` then `tabulateM` twice and use the provided operation to
-- combine the keys
duplicateRepMBy :: RepresentableM m f => (RepM f -> RepM f -> RepM f) -> f a -> f (f a)
duplicateRepMBy plus w = tabulateM (\k -> return $ tabulateM (indexM w . plus k))

-- | `indexM` then `tabulateM` twice and use the provided operation to
-- combine the keys
extendRepMBy :: RepresentableM m f => (RepM f -> RepM f -> RepM f) -> (f a -> b) -> f a -> f b
extendRepMBy plus f w = tabulateM (\k -> return $ f (tabulateM (indexM w . plus k)))

-- | Since we need to return a raw value, @m@ must be a `Comonad`
extractRepMBy :: (RepresentableM m f, Comonad m) => RepM f -> f a -> a
extractRepMBy = fmap extract . flip indexM


-- WithIndex

-- | `tabulateM` to expose the `RepM` and then `fmap`
imapRepM :: RepresentableM m f => (RepM f -> a -> b) -> f a -> f b
imapRepM f xs = tabulateM $ \k -> f k <$> indexM xs k

-- | `tabulateM` to expose the `RepM` and then (`>>=`)
imapMRepM :: RepresentableM m f => (RepM f -> a -> m b) -> f a -> f b
imapMRepM f xs = tabulateM $ \k -> indexM xs k >>= f k

-- | Indexed `foldMap`
ifoldMapRepM :: forall m f a b. (RepresentableM m f, Foldable f, Monoid b) => (RepM f -> a -> b) -> f a -> b
ifoldMapRepM ix xs = fold ((tabulateM $ \(k :: RepM f) -> ix k <$> indexM xs k) :: f b)

-- | Indexed `traverse`
itraverseRepM :: (RepresentableM m f, Traversable f, Applicative g) => (RepM f -> a -> g b) -> f a -> g (f b)
itraverseRepM ix xs = sequenceA . tabulateM $ \k -> ix k <$> indexM xs k


-- MonadFree

-- | Wrap the base `Monad` of a `RepresentableM`
wrapRepM :: RepresentableM m f => m (f a) -> f a
wrapRepM m = tabulateM $ \k -> m >>= (`indexM` k)


-- Lifting other operations

-- | Lift the base `Monad` of a `RepresentableM`
liftRepM :: RepresentableM m f => m a -> f a
liftRepM = tabulateM . const

-- | Lift a @callCC@ operation to the base `Monad`.
liftCallCC :: RepresentableM m f
           => (((a -> m b) -> m a) -> m a) -- ^ @callCC@ on the base `Monad`.
           -> ((a -> f b) -> f a)
           -> f a
liftCallCC callCC f =
  tabulateM $ \k -> callCC $ \c -> indexM (f (tabulateM . const . c)) k

-- | Lift a @catchError@ operation to the base monad.
liftCatch :: RepresentableM m f
          => (m a -> (RepM f -> m a) -> m a) -- ^ @catch@ on the base monad.
          -> f a                             -- ^ Computation to attempt.
          -> (RepM f -> f a)                 -- ^ Exception handler.
          -> f a
liftCatch catch m handle =
  tabulateM $ \k -> indexM m k `catch` \e -> indexM (handle e) k


-- Generics

-- | A default implementation of `tabulateM` in terms of `GRepM`.
gtabulateM ::
     (Generic1 f, GRepM f ~ RepM f, GTabulateM m (Rep1 f))
  => (RepM f -> m a)
  -> f a
gtabulateM = to1 . gtabulateM'

-- | A default implementation of `indexM` in terms of `GRepM`.
gindexM ::
     (Generic1 f, GRepM f ~ RepM f, GIndexM m (Rep1 f))
  => f a
  -> (RepM f -> m a)
gindexM = gindexM' . from1

-- | A convenient composition of `GRepM'` and `Rep1`
type GRepM f = GRepM' (Rep1 f)

-- | Generic `RepM`, with a customer `TypeError` for unsupported types.
type family GRepM' (f :: * -> *) :: * where
  GRepM' (f :*: g) = Either (GRepM' f) (GRepM' g)
  GRepM' Par1 = ()
  GRepM' (M1 i c f) = GRepM' f
  GRepM' (f :.: g) = (WrappedRepM f, GRepM' g)
  GRepM' (Rec1 f) = WrappedRepM f
  GRepM' U1 = Void
  GRepM' x = TypeError (
      'Text "The type " ':<>:
      'ShowType x ':<>:
      'Text " is not one of (:*:), (:.:), Par1, M1, Rec1, or U1."
    )

-- | The typeclass implementing `gtabulateM` (as `gtabulateM'`)
-- for `Generic` types
class Monad m => GTabulateM (m :: * -> *) (f :: * -> *) | f -> m where
  gtabulateM' :: (GRepM' f -> m a) -> f a

instance (GTabulateM m f, GTabulateM m g) => GTabulateM m (f :*: g) where
  gtabulateM' f = gtabulateM' (f . Left) :*: gtabulateM' (f . Right)

instance GTabulateM Identity Par1 where
  gtabulateM' = Par1 . runIdentity . extract

instance GTabulateM m f => GTabulateM m (M1 i c f) where
  gtabulateM' = M1 #. gtabulateM'

instance (RepresentableM m f, GTabulateM m g) => GTabulateM m (f :.: g) where
  gtabulateM' f =
    Comp1 $ tabulateM $ fmap (return . gtabulateM') $ fmap (curry f) WrapRepM

instance RepresentableM m f => GTabulateM m (Rec1 f) where
  gtabulateM' = Rec1 #. tabulateM .# (. WrapRepM)

-- | The monadic side-effects must be thrown out,
-- so the `Monad` must be `Identity`
instance GTabulateM Identity U1 where
  gtabulateM' _ = U1


-- | The typeclass implementing `gindexM` (as `gindexM'`)
-- for `Generic` types
class Monad m => GIndexM (m :: * -> *) (f :: * -> *) | f -> m where
  gindexM' :: f a -> (GRepM' f -> m a)

instance (GIndexM m f, GIndexM m g) => GIndexM m (f :*: g) where
  gindexM' ~(a :*: _)  (Left  i) = gindexM' a i
  gindexM' ~(_ :*: b) ~(Right j) = gindexM' b j

instance GIndexM Identity Par1 where
  gindexM' (Par1 a) () = Identity a

instance GIndexM m f => GIndexM m (M1 i c f) where
  gindexM' = gindexM' .# unM1

instance (RepresentableM m f, GIndexM m g) => GIndexM m (f :.: g) where
  gindexM' (Comp1 fg) ~(i, j) = (indexM fg .# unwrapRepM) i >>= (`gindexM'` j)

instance RepresentableM m f => GIndexM m (Rec1 f) where
  gindexM' = (. unwrapRepM) #. indexM .# unRec1

-- `absurd`
instance GIndexM Identity U1 where
  gindexM' U1 = absurd


-- | `WrappedRepM` prevents generic `RepresentableM` instances for
-- recursive types from sending the typechecker into an infinite loop.
--
-- See `WrappedRep`.
newtype WrappedRepM f = WrapRepM { unwrapRepM :: RepM f }

deriving instance Bits (RepM f) => Bits (WrappedRepM f)

deriving instance Bounded (RepM f) => Bounded (WrappedRepM f)

deriving instance (Data (RepM f), Typeable f) => Data (WrappedRepM f)

deriving instance Enum (RepM f) => Enum (WrappedRepM f)

deriving instance Eq (RepM f) => Eq (WrappedRepM f)

deriving instance FiniteBits (RepM f) => FiniteBits (WrappedRepM f)

deriving instance Floating (RepM f) => Floating (WrappedRepM f)

deriving instance Fractional (RepM f) => Fractional (WrappedRepM f)

deriving instance Generic (RepM f) => Generic (WrappedRepM f)

deriving instance Ix (RepM f) => Ix (WrappedRepM f)

deriving instance Monoid (RepM f) => Monoid (WrappedRepM f)

deriving instance Num (RepM f) => Num (WrappedRepM f)

deriving instance Ord (RepM f) => Ord (WrappedRepM f)

deriving instance PrintfArg (RepM f) => PrintfArg (WrappedRepM f)

deriving instance Read (RepM f) => Read (WrappedRepM f)

deriving instance Real (RepM f) => Real (WrappedRepM f)

deriving instance RealFloat (RepM f) => RealFloat (WrappedRepM f)

deriving instance RealFrac (RepM f) => RealFrac (WrappedRepM f)

deriving instance Semigroup (RepM f) => Semigroup (WrappedRepM f)

deriving instance Show (RepM f) => Show (WrappedRepM f)

deriving instance Storable (RepM f) => Storable (WrappedRepM f)

deriving instance Typeable (RepM f) => Typeable (WrappedRepM f)

-- | `Cofree`, `CofreeT` instances
todo :: ()
todo = ()

