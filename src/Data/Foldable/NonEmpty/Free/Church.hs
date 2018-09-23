{-# LANGUAGE RankNTypes #-}

{-| This module provides an implementation for a free `Foldable1` using
    a Church encoding.
-}

module Data.Foldable.NonEmpty.Free.Church where

-- | Church encoding of a free `Foldable1`
newtype Foldable1F a = Foldable1F { runFoldable1F :: forall x y. a -> (x -> a -> x) -> x -> (x -> y) -> y }

