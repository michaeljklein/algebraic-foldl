
{-| This module provides a `Fold1` that returns a non-empty stream of outputs
-}

module Control.Foldls.NonEmpty where

import Control.Foldl.NonEmpty (Fold1(..))
import Data.Foldable.NonEmpty.Free (FoldableFree1(..))

-- | A `Fold1` returning a non-empty stream of outputs
newtype Folds1 a b = Folds1 { runFolds1 :: Fold1 a (FoldableFree1 b) }

