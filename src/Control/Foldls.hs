{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

{-| This module provides a `Fold` that returns a stream of outputs

   Note that it's a `Category`.
-}

module Control.Foldls where

import Data.Semigroupoid (Semigroupoid(..))
import Control.Category (Category(..))
import Prelude hiding ((.), id)
import Control.Foldl (Fold(..), FoldM(..))
import Data.Foldable.Free (FoldableFree(..), foldableFree)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Functor.Rep (Representable(..))

-- | A `Fold` returning a stream of outputs.
--
--  @
--   `Folds` a b = `Fold` a (`Rep` (`Fold` b))
--   `Folds` a b = `Fold` a (`FoldableFree` b)
--  @
--
newtype Folds a b = Folds { runFolds :: Fold a (FoldableFree b) } deriving (Functor)

-- | Composition of `Folds` is natural, once you notice that
-- @`FoldableFree` b ~ `Rep` (`Fold` b)@:
--
-- @
--   `Folds` f \``o`\` `Folds` g =
--     `Folds` . `tabulate` $
--       `index` f . `index` g
-- @
--
instance Semigroupoid Folds where
  o :: Folds b c -> Folds a b -> Folds a c
  o ~(Folds f) ~(Folds g) =
    Folds . tabulate $
      index f . index g

-- | `id` is provided by `foldableFree`,
-- which folds values into `FoldableFree`.
instance Category Folds where
  id :: Folds a a
  id = Folds foldableFree

  (.) = o

-- | `undefined`
readerToFolds :: (forall x. FoldM (ReaderT x (Fold b)) a x) -> Folds a b
readerToFolds _ = undefined

-- | `undefined`
foldsToReader :: Folds a b -> (forall x. FoldM (ReaderT x (Fold b)) a x)
foldsToReader = undefined

-- | Simply @(`return` :: b -> `FoldableFree` b)@ the result of a `Fold`
foldToFolds :: Fold a b -> Folds a b
foldToFolds = Folds . fmap return

