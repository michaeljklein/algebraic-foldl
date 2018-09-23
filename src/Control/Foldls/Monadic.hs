{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module provides a `FoldM` that returns a monadic stream of outputs
-}

module Control.Foldls.Monadic where

import Control.Category (Category(..))
import Control.Comonad (Comonad(..))
import Control.Foldl (FoldM(..))
import Control.Foldl.Utils (unwrapFoldM)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Free.Class (MonadFree(..))
import Control.Monad.Rep
  ( RepresentableM(..)
  , apRepM
  , bindRepM
  , duplicatedRepM
  , mfixRepM
  , mzipWithRepM
  , pureRepM
  )
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Distributive.Monadic (DistributiveM(..), collectmByDistributem)
import Data.Foldable.Monadic (FoldableM(..))
import Data.Foldable.Monadic.Free
  ( FoldableFreeM(..)
  , foldableFreeM
  , unwrapFoldableFreeM
  , wrapFoldM
  )
import Data.Functor.Extend (Extend(..))
import Data.Profunctor (Profunctor(..))
import Data.Semigroupoid (Semigroupoid(..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits
import Prelude hiding ((.), id)
-- import Data.Distributive (Distributive(..)) -- I don't think this has an instance, see: FoldsM1, it should guarantee a Comonad instance at least

-- | A `FoldM` returning a `Monad`ic stream of outputs.
--
--  @
--   `FoldsM` m a b = `FoldM` m a (`RepM` (`FoldM` m b))
--   `FoldsM` m a b = `FoldM` m a (`FoldableFreeM` b)
--  @
--
newtype FoldsM m a b = FoldsM
  { runFoldsM :: FoldM m a (FoldableFreeM m b)
  } deriving (Generic, Generic1, Functor)

instance Monad m => Applicative (FoldsM m a) where
  pure = pureRepM
  (<*>) = apRepM

instance Monad m => Monad (FoldsM m a) where
  (>>=) = bindRepM

instance Monad m => DistributiveM (FoldableFreeM m) (FoldsM m a) where
  distributem = tabulateM . flip (fmap . dimap runFoldsM wrap . flip impureFold)
  collectm = collectmByDistributem

instance Monad m => RepresentableM (FoldableFreeM m) (FoldsM m a) where
  type RepM (FoldsM m a) = FoldableFreeM m a
  indexM = fmap wrap . impureFold . runFoldsM
  tabulateM = FoldsM . tabulateM . fmap return

instance Monad m => MonadFix (FoldsM m a) where
  mfix = mfixRepM

instance Monad m => MonadZip (FoldsM m a) where
  mzipWith = mzipWithRepM

instance Monad m => Extend (FoldsM m a) where
  duplicated = duplicatedRepM

instance (Monad m, TypeError ('Text "This instance requires: Comonad (FoldableFreeM m)")) => Comonad (FoldsM m a) where
  duplicate = error "duplicatedRepM"
  extract = error "extractRepM"

instance Monad m => MonadFree m (FoldsM m a) where
  wrap = FoldsM . wrapFoldM . fmap runFoldsM

-- | Unwrap a layer of the `FoldsM`'s given `Monad`, e.g. to catch
-- an exception before continuing.
unwrapFoldsM :: Monad m => FoldsM m a b -> m (FoldsM m a b)
unwrapFoldsM = fmap FoldsM . unwrapFoldM . runFoldsM

-- | `undefined`
readerToFoldsM :: Monad m => (forall x. FoldM (ReaderT x (FoldM m b)) a x) -> FoldsM m a b
readerToFoldsM _ = undefined

-- | `undefined`
foldsMToReader :: Monad m => FoldsM m a b -> (forall x. FoldM (ReaderT x (FoldM m b)) a x)
foldsMToReader _ = undefined

-- | Simply @(`return` :: b -> `FoldableFreeM` m b)@ the result of a `FoldM`
foldMToFoldsM :: Monad m => FoldM m a b -> FoldsM m a b
foldMToFoldsM = FoldsM . fmap return

-- | Composition of `FoldsM` is natural, once you notice that
-- @`FoldableFreeM` m b ~ `RepM` (`FoldM` m b)@ and that we can
-- wrap and unwrap `FoldableFreeM`:
--
-- @
--  `wrap` :: `Monad` m => m (`FoldableFreeM` m a) -> `FoldableFreeM` m a
--  `unwrapFoldableFreeM` :: `Monad` m => `FoldableFreeM` m a -> m (`FoldableFreeM` m a)
--
--   `FoldsM` f \``o`\` `FoldsM` g =
--     `FoldsM` . `tabulateM` . `fmap` `unwrapFoldableFreeM` $
--       `fmap` `wrap` (`indexM` f) . `fmap` `wrap` (`indexM` g)
-- @
--
instance Monad m => Semigroupoid (FoldsM m) where
  o ~(FoldsM f) ~(FoldsM g) =
    FoldsM . tabulateM . fmap unwrapFoldableFreeM $
      fmap wrap (indexM f) . fmap wrap (indexM g)

-- | `id` is simply:
--
-- @
--  `id` :: `FoldsM` m a a
--  `id` = `FoldsM` `foldableFreeM`
--
instance Monad m => Category (FoldsM m) where
  id :: FoldsM m a a
  id = FoldsM foldableFreeM

  (.) = o

