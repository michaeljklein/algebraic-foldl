
{-| This module defines some utilities for `Either`
-}

module Data.Either.Utils
  ( joinEither
  , combineLefts
  ) where

-- | Unwrap an `Either` with equal types
{-# INLINE joinEither #-}
joinEither :: Either a a -> a
joinEither (Left x) = x
joinEither ~(Right x) = x

-- | Combine `Left`s using the provided function
{-# INLINE combineLefts #-}
combineLefts ::
     (a -> b -> c)
  -> Either a d
  -> Either b e
  -> Either c (Either a d, Either b e)
combineLefts f (Left x) (Left y) = Left $ f x y
combineLefts _ x y = Right (x, y)

