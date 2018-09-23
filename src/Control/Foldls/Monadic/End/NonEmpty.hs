
{-| This module provides a `FoldEM1` that returns a non-empty monadic stream of outputs
-}

module Control.Foldls.Monadic.End.NonEmpty where

import Control.Foldl.Monadic.End.NonEmpty (FoldEM1(..))
import Data.Foldable.Monadic.NonEmpty.Free (FoldableFreeM1(..))
import Control.Monad.Trans.Except (ExceptT(..))

-- | A `FoldEM1` returning a monadic stream of outputs
newtype FoldsEM1 m a b = FoldsEM1 { runFoldsEM :: FoldEM1 m a (FoldableFreeM1 (ExceptT b m) b) }

