{-# LANGUAGE RecordWildCards #-}

{-| This module provides a class for folding over non-empty containers,
    i.e. a subclass of `Foldable`.
-}

module Data.Foldable.NonEmpty where

import Control.Comonad.Cofree (Cofree(..))
import Control.Foldl (Fold(..), FoldM(..), fold, foldM, generalize, impurely)
import Control.Foldl.End (FoldE(..))
import Control.Foldl.End.NonEmpty (FoldE1(..))
import Control.Foldl.Monadic.End (FoldEM(..))
import Control.Foldl.Monadic.End.NonEmpty (FoldEM1(..))
import Control.Foldl.Monadic.NonEmpty (FoldM1(..))
import qualified Control.Foldl.Monadic.NonEmpty as FM1 (foldM1)
import Control.Foldl.NonEmpty (Fold1(..))
import qualified Control.Foldl.NonEmpty as F1 (fold1NonEmpty, head1, last1)
import Control.Monad.Free (Free(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.NonEmpty (NonEmptyT(..))
import Data.List.NonEmpty (NonEmpty(..))


-- | Those data-types that can be folded over and are non-empty
class Foldable t => Foldable1 t where
  -- | Apply a pure strict left fold to a non-empty collection
  fold1 :: Fold1 a b -> t a -> b
  fold1 =
    fmap runIdentity .
    foldM1 . FoldM1 . ReaderT . fmap generalize . runReaderT . runFold1

  -- | Apply a `Monad`ic strict left fold to a non-empty collection
  foldM1 :: Monad m => FoldM1 m a b -> t a -> m b
  foldM1 = fold1 . asPure1

  -- | The first element of a non-empty collection
  head1 :: t a -> a
  head1 = fold1 F1.head1

  -- | The last element of a non-empty collection
  last1 :: t a -> a
  last1 = fold1 F1.last1

  {-# MINIMAL fold1 | foldM1 #-}

-- | Return `empty` unless the container being folded over is non-empty.
-- Otherwise behaves like `fold1`.
--
-- `undefined`
pureFold1Maybe :: Foldable t => Fold1 a b -> t a -> MaybeT m b
pureFold1Maybe = undefined

-- | Return `empty` unless the container being folded over is non-empty.
-- Otherwise behaves like `foldM1`.
--
-- `undefined`
impureFold1Maybe :: Foldable t => Fold1 a b -> t a -> MaybeT m b
impureFold1Maybe = undefined

-- | Convert a `FoldM` to a `Fold` that returns a `Monad`ic value
asPure :: Monad m => FoldM m a b -> Fold a (m b)
asPure f = (`impurely` f) $ \stp ini ext -> Fold (\x y -> x >>= (`stp` y)) ini (>>= ext)

-- | Convert a `FoldM1` to a `Fold1` that returns a `Monad`ic value
asPure1 :: Monad m => FoldM1 m a b -> Fold1 a (m b)
asPure1 FoldM1{..} = Fold1 . ReaderT $ \x -> asPure (runReaderT runFoldM1 x)

-- | Convert a `FoldEM` to a `FoldE` that returns a `Monad`ic value
--
-- `undefined`
asPureE :: Monad m => FoldEM m a b -> FoldE a (m b)
asPureE = undefined

-- | Convert a `FoldEM1` to a `FoldE1` that returns a `Monad`ic value
--
-- `undefined`
asPureE1 :: Monad m => FoldEM1 m a b -> FoldE1 a (m b)
asPureE1 = undefined


instance Foldable1 NonEmpty where
  fold1 = F1.fold1NonEmpty
  foldM1 = FM1.foldM1

instance Foldable t => Foldable1 (NonEmptyT t) where
  fold1 Fold1{..} ~(NonEmptyT (x, xs)) = fold (runReaderT runFold1 x) xs
  foldM1 FoldM1{..} ~(NonEmptyT (x, xs)) = foldM (runReaderT runFoldM1 x) xs

-- | `undefined`
instance Foldable f => Foldable1 (Cofree f) where
  fold1 = undefined

-- | `undefined`
instance Foldable1 f => Foldable1 (Free f) where
  fold1 = undefined

