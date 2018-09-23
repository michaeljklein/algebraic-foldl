
{-| This module provides a `FoldE` that returns a stream of outputs
-}

module Control.Foldls.End where

import Control.Foldl.End (FoldE(..))
import qualified Control.Foldl as F
import Data.Foldable.Monadic.Free (FoldableFreeM(..))
import Data.Profunctor
import Data.Bifunctor

-- | A `FoldE` that returns a non-empty stream of outputs
newtype FoldsE a b = FoldsE { runFoldsE :: FoldE a (FoldableFreeM (Either b) b) }

-- | Right map over a `FoldsE`
rmapFoldsE :: (a -> b) -> (b -> a) -> FoldsE t a -> FoldsE t b
rmapFoldsE f g = FoldsE . fmap (\h -> FoldableFreeM $ dimap (F.hoists (first g) . lmap f) (first f) (runFoldableFreeM h) ) . runFoldsE

-- | Left map over a `FoldsE`
lmapFoldsE :: (a -> b) -> FoldsE b c -> FoldsE a c
lmapFoldsE f = FoldsE . lmap f . runFoldsE

