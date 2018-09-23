{-# LANGUAGE RankNTypes #-}

{-| This module provides an implementation of a free `FoldableM1`
    in terms of `FoldM1`.
-}

module Data.Foldable.Monadic.NonEmpty.Free where

import Control.Foldl.Monadic.NonEmpty (FoldM1(..))

-- | Encoding of a free `FoldableM1`
newtype FoldableFreeM1 m a = FoldableFreeM1 { runFoldableFreeM1 :: forall x. FoldM1 m a x -> x }

