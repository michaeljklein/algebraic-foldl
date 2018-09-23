{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

{-| This module provides a version of `FoldM` that can return early
-}

module Control.Foldl.Monadic.End where

import Control.Monad (liftM2)
import Control.Comonad.Cofree (Cofree(..))
import Control.Foldl
import Control.Foldl.Free
import Control.Foldl.End (FoldE(..))
import Control.Foldl.Free (FreeFoldM(..), freeFoldM)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Either.Utils (joinEither)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Extend (Extend(..))
import Data.Profunctor (Profunctor(..))
import Data.Semigroupoid (Semigroupoid(..))
import Control.Category (Category(..))
import Prelude hiding ((.), id, fail)
import Control.Monad.Fail (MonadFail(..))

-- | `FoldM` left or return an early result
newtype FoldEM m a b = FoldEM
  { runFoldEM :: FoldM (ExceptT b m) a b
  }

instance Monad m => Functor (FoldEM m a) where
  fmap f = FoldEM . hoists (withExceptT f) . fmap f . runFoldEM

instance Monad m => Profunctor (FoldEM m) where
  lmap f = FoldEM . lmap f . runFoldEM
  rmap = fmap

instance Monad m => Apply (FoldEM m a) where
  fs <.> xs = foldEMThenEMRec fs (<$> xs)

instance Monad m => Applicative (FoldEM m a) where
  pure x =
    FoldEM $ FoldM (\_ _ -> throwError x) (throwError x) (const $ throwError x)
  (<*>) :: FoldEM m a (b -> c) -> FoldEM m a b -> FoldEM m a c
  (<*>) = (<.>)

instance Monad m => Monad (FoldEM m a) where
  (>>=) = foldEMThenEMRec

-- | Note: the `return` is due to the fact that we only know how to return
-- early from the original `FoldEM`.
instance Monad m => Extend (FoldEM m a) where
  duplicated =
    FoldEM . fmap FoldEM . hoists (withExceptT return) . duplicated . runFoldEM

-- | Since `FoldEM` can return before consuming the entire input,
-- we can repeat it on the input and fold over its results using
-- the composed `FoldEM` (the first argument to `o`).
instance Monad m => Semigroupoid (FoldEM m) where
  o :: FoldEM m b c -> FoldEM m a b -> FoldEM m a c
  o (FoldEM f) (FoldEM g) =
    (`impurely` f) $ \stpF iniF extF ->
      (`impurely` g) $ \stpG (ExceptT iniG) extG ->
        let iniGR = (\(~(Right x)) -> x) <$> lift iniG
         in FoldEM $
            FoldM
              (\(~(x, y)) z -> do
                 y' <- lift . runExceptT $ stpG y z
                 case y' of
                   Left yL -> do
                     iniGR' <- iniGR
                     (, iniGR') <$> stpF x yL
                   ~(Right yR) -> return (x, yR))
              (do iniF' <- iniF
                  iniG' <- lift iniG
                  case iniG' of
                    Left iniGL -> stpF iniF' iniGL >>= extF >>= throwError
                    ~(Right iniGR') -> return (iniF', iniGR'))
              (\(~(x, y)) ->
                 (lift . runExceptT . extG) y >>= stpF x . joinEither >>= extF)

-- | Because we could fold over an empty container, we need an instance
-- of `MonadFail` to handle that case for `id`.
instance MonadFail m => Category (FoldEM m) where
  id :: FoldEM m a a
  id =
    FoldEM . FoldM (return throwError) (return ()) . const . ExceptT $
    fail "id :: FoldEM m a a: empty input"

  (.) = o

-- | Unfold the successive applications of a `FoldEM` into a `Cofree` `ReaderT` `ExceptT`
toCofreeReaderM ::
     Monad m => FoldEM m a b -> ExceptT b m (Cofree (ReaderT a (ExceptT b m)) b)
toCofreeReaderM (FoldEM f) =
  (`impurely` f) $ \stp (ExceptT ini) ext -> do
    ini' <- lift ini
    let step' =
          liftM2 (liftM2 (:<)) ext (return . ReaderT . fmap (>>= step') . stp)
    either throwError step' ini'

-- | Unfold a `FoldEM` into a `Cofree` `ReaderT`
toCofreeReaderT ::
     Monad m => FoldEM m a b -> ExceptT b m (Cofree (ReaderT a (ExceptT b m)) b)
toCofreeReaderT (FoldEM f) =
  (`impurely` f) $ \stp ini ext -> do
    ini' <- ini
    let step' x = do
          x' <- lift . joinExceptT . ext $ x
          return $ x' :< ReaderT ((>>= step') . stp x)
    step' ini'

-- | Convert a `FoldE` to a `FreeFoldM` using `toCofreeReaderT`
{-# INLINE foldEMToFree #-}
foldEMToFree :: Monad m => FoldEM m a b -> FreeFoldM (ExceptT b m) a b
foldEMToFree = FreeFoldM . toCofreeReaderT

-- | using `foldFreeThenFreeM` instead of directly recursing
{-# INLINE foldEMThenFree #-}
foldEMThenFree ::
     Monad m
  => FoldEM m a b
  -> (b -> FoldEM m a c)
  -> FreeFoldM (ExceptT c m) a c
foldEMThenFree f g = foldFreeThenFreeM (foldEMToFree f) (foldEMToFree . g)

-- | Using `FreeFoldM`, fold with the first `FoldEM` until it returns,
-- then use its result to generate the second `FoldEM` and continue.
{-# INLINE foldEMThenEMRec #-}
foldEMThenEMRec ::
     Monad m => FoldEM m a b -> (b -> FoldEM m a c) -> FoldEM m a c
foldEMThenEMRec f = FoldEM . freeFoldM . foldEMThenFree f

-- | Lift a `FoldE` to a `FoldEM` by `return`ing the results
liftFoldE :: Monad m => FoldE a b -> FoldEM m a b
liftFoldE (FoldE f) = FoldEM $ hoists (ExceptT . return) f

-- | We can convert a `FoldEM` to a `FoldM`
-- (without the `ExceptT` provided by `runFoldEM`)
-- by including the `Either` in the state and simply
-- extracting the early return value.
fromFoldEM :: Monad m => FoldEM m a b -> FoldM m a b
fromFoldEM =
  impurely
    (\stp ini ext ->
       FoldM
         (\x y -> either (return . Left) (runExceptT . flip stp y) x)
         (runExceptT ini)
         (either return (fmap joinEither . runExceptT . ext))) .
  runFoldEM

-- | We can lift a `FoldM` to a `FoldEM` without the need to explicitly
-- specify the required `ExceptT`, by using `liftExceptT`.
liftFoldEM :: MonadError b m => FoldM m a b -> FoldEM m a b
liftFoldEM = FoldEM . hoists liftExceptT


-- Helpers

-- | Lift an instance of `MonadError` to `ExceptT` using `catchError`.
liftExceptT :: MonadError e m => m a -> ExceptT e m a
liftExceptT xs = ExceptT $ (Right <$> xs) `catchError` (return . Left)

-- | Join the error and result terms of an `ExceptT`
{-# INLINE joinExceptT #-}
joinExceptT :: Functor m => ExceptT a m a -> m a
joinExceptT = fmap joinEither . runExceptT

