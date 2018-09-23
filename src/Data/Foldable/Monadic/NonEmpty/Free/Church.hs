{-# LANGUAGE RankNTypes #-}

{-| This module provides an implementation of a free `FoldableM1`
    with a Church encoding.
-}

module Data.Foldable.Monadic.NonEmpty.Free.Church where

-- | Church encoding of a free `FoldableM`
newtype FoldableM1F m a = FoldableM1F { runFoldableM1F :: forall x y. a -> (x -> a -> m x) -> x -> (x -> m y) -> m y }

