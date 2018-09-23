{-# LANGUAGE RankNTypes #-}

{-| This module provides an implementation for a free `Foldable1` in
    terms of `Fold1`.
-}

module Data.Foldable.NonEmpty.Free where

import Control.Foldl.NonEmpty (Fold1(..))

-- | Encoding of a free `Foldable1`
newtype FoldableFree1 a = FoldableFree1 { runFoldableFree1 :: forall x. Fold1 a x -> x }

