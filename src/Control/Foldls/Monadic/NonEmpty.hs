{-| This module provides a `FoldM1` that returns a non-empty monadic stream of outputs
-}

module Control.Foldls.Monadic.NonEmpty where

import Control.Foldl.Monadic.NonEmpty (FoldM1(..))
import Data.Foldable.Monadic.NonEmpty.Free (FoldableFreeM1(..))

-- | A `FoldM1` returning a non-empty monadic stream of outputs
newtype FoldsM1 m a b = FoldsM1 { runFoldsM :: FoldM1 m a (FoldableFreeM1 m b) }

