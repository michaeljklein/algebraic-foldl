{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This module provides utilities for working with `Fold` and `FoldM`
-}

module Control.Foldl.Utils where

import Control.Foldl
  ( Fold(..)
  , FoldM(..)
  , duplicateM
  , fold
  , foldM
  , impurely
  , generalize
  , purely
  )
import Control.Monad (join)
import Data.Functor.Extend (Extend(..))
import Data.Functor.Identity (Identity(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

-- | Extract the monadic result from a `FoldM`
{-# INLINE extractM #-}
{-# SPECIALIZE extractM :: FoldM Identity a b -> Identity b #-}
{-# SPECIALIZE extractM :: FoldM (Either b) a b -> Either b b #-}
extractM :: Monad m => FoldM m a b -> m b
extractM = impurely $ const (>>=)

-- | Produce a `FoldM` that ignores all input and returns
-- the given monadic value.
pureM :: Monad m => m b -> FoldM m a b
pureM x = FoldM (fmap return . const) x return

-- | Convert a `Fold` to a function
{-# INLINE toFunction #-}
toFunction :: Fold a b -> a -> b
toFunction = (. Identity) . fold

-- | Convert a `FoldM` to a function
{-# INLINE toFunctionM #-}
{-# SPECIALIZE toFunctionM :: FoldM Identity a b -> a -> Identity b #-}
{-# SPECIALIZE toFunctionM :: FoldM (Either b) a b -> a -> Either b b #-}
toFunctionM :: Monad m => FoldM m a b -> a -> m b
toFunctionM = (. Identity) . foldM

-- | @`duplicated` = `duplicateM`@
instance Monad m => Extend (FoldM m a) where
  {-# INLINE duplicated #-}
  {-# SPECIALIZE duplicated :: FoldM Identity a b -> FoldM Identity a (FoldM Identity a b) #-}
  {-# SPECIALIZE duplicated :: FoldM (Either b) a b -> FoldM (Either b) a (FoldM (Either b) a b) #-}
  duplicated = duplicateM

-- | Perform a single step of a `Fold`
{-# INLINE stepFold #-}
stepFold :: a -> Fold a b -> Fold a b
stepFold x f = (`purely` f) $ \stp ini -> Fold stp (ini `stp` x)

-- | `stepFold`, generalized to `FoldM`
{-# INLINE stepFoldM #-}
{-# SPECIALIZE stepFoldM :: a -> FoldM Identity a b -> FoldM Identity a b #-}
{-# SPECIALIZE stepFoldM :: a -> FoldM (Either b) a b -> FoldM (Either b) a b #-}
stepFoldM :: Monad m => a -> FoldM m a b -> FoldM m a b
stepFoldM x f = (`impurely` f) $ \stp ini -> FoldM stp (ini >>= (`stp` x))

-- | Expose the monadic context of the current state of a `FoldM`.
-- Can be used, for example, to catch errors before continuing a fold.
{-# INLINE unwrapFoldM #-}
{-# SPECIALIZE unwrapFoldM :: FoldM Identity a b -> Identity (FoldM Identity a b) #-}
{-# SPECIALIZE unwrapFoldM :: FoldM (Either b) a b -> Either b (FoldM (Either b) a b) #-}
unwrapFoldM :: Applicative m => FoldM m a b -> m (FoldM m a b)
unwrapFoldM f = (`impurely` f) $ \stp ini ext -> (\ini' -> FoldM stp (pure ini') ext) <$> ini

-- | Join a monadic result into the monadic context of the `FoldM`
innerJoinFoldM :: Monad m => FoldM m a (m b) -> FoldM m a b
innerJoinFoldM = impurely $ \stp ini ext -> FoldM stp ini (join . ext)

-- | Generalize a `Fold` returning `Maybe` to `MaybeT`.
--
-- For example:
--
-- @
--  λ> F.foldM (generalizeMaybe F.head) [1..10] :: MaybeT Identity Int
--  MaybeT (Identity (Just 1))
-- @
--
generalizeMaybe :: Monad m => Fold a (Maybe b) -> FoldM (MaybeT m) a b
generalizeMaybe = innerJoinFoldM . generalize . fmap (MaybeT . return)

-- | Count the number of elements that match a predicate
--
-- Should be equivalent to:
-- length . prefilter p
{-# INLINE count #-}
{-# SPECIALIZE count :: (a -> Bool) -> Fold a Int #-}
{-# SPECIALIZE count :: (a -> Bool) -> Fold a Integer #-}
count :: Enum i => (a -> Bool) -> Fold a i
count p =
  Fold
    (\x y ->
       if p y
         then (toEnum . (+ 1) . fromEnum) $ x
         else x)
    (toEnum 0)
    id

-- | `count` using a monadic predicate
{-# INLINE countM #-}
{-# SPECIALIZE countM :: Enum i => (a -> Identity Bool) -> FoldM Identity a i #-}
{-# SPECIALIZE countM :: Enum i => (a -> Either i Bool) -> FoldM (Either i) a i #-}
{-# SPECIALIZE countM :: (a -> Identity Bool) -> FoldM Identity a Int #-}
{-# SPECIALIZE countM :: (a -> Either Int Bool) -> FoldM (Either Int) a Int #-}
{-# SPECIALIZE countM :: (a -> Identity Bool) -> FoldM Identity a Integer #-}
{-# SPECIALIZE countM :: (a -> Either Integer Bool) -> FoldM (Either Integer) a Integer #-}
countM :: (Enum i, Monad m) => (a -> m Bool) -> FoldM m a i
countM p =
  FoldM
    (\x y -> do
       p' <- p y
       if p'
         then return . (toEnum . (+ 1) . fromEnum) $ x
         else return x)
    (return $ toEnum 0)
    return

