{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-| This module provides a version of `Foldl` that can return early
-}

module Control.Foldl.End (
  -- * Data type declarations
    FoldE(..)

  -- * Data type helpers
  , withFoldE
  , fmapAlternative
  , stepFoldE
  , unwrapFoldE

  -- * Folds
  , takeE
  , dropE
  , lineE
  , headE
  , tailE
  , lastE

  , product
  , elem
  , elem'
  , find
  , index
  , lookup
  , minimum
  , maximum
  , elemIndex
  , findIndex
  , genericIndex

  -- * Ordered folds
  , lookupOrd
  , elemIndexOrd

  -- * Conversion
  , fromFoldE
  , toFoldE
  , foldE1
  , toCofreeReader
  , foldEToFree

  -- * Folding
  , foldE

  -- * Combinators
  , foldEThenE
  , foldEThenFree
  , foldEThenERec
  , foldEThenERec'
  , foldEThenE_
  , foldEThen
  , foldEThen_
  , peekLast
  , peekLast_
  , foldE2
  , foldERec2

  -- * Predicate combinators
  , countPrefixE
  , takeWhileE
  , foldWhile
  , foldUntil
  , foldWhileE
  , foldUntilE
  , foldWhile2
  , bifoldWhile
  , foldWhileE2
  , bifoldWhileE
  , skipWhile
  , skipUntil
  , satisfyE
  , prefilterMaybe

  -- * Free predicate combinators
  , freeToFoldE
  , bifoldWhileE'
  , bifoldWhileFreeE
  , foldWhileFreeE

  -- * Container predicates
  , null
  , and
  , or
  , all
  , any

  -- * Series predicates
  , eq
  , inc
  , inc'
  , dec
  , dec'
  ) where

import Control.Applicative
import Control.Category (Category(..))
import Control.Comonad
import Control.Comonad.Cofree
import Control.Foldl (Fold(..), FoldM(..), impurely, purely)
import qualified Control.Foldl as F
import Control.Foldl.Free
  ( FreeFoldM(..)
  , foldFree2
  , toFreeFold
  , foldFreeThenFree
  , freeFoldM
  )
import Control.Foldl.Utils (stepFoldM, unwrapFoldM)
import Control.Monad hiding (foldM, join)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Trans.Reader (ReaderT(..), Reader)
import Control.Monad.Zip (MonadZip(..))
import Data.Bifunctor
import Data.Either
import Data.Either.Utils (combineLefts, joinEither)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Functor.Extend (Extend(..))
import Data.Functor.Identity
import Data.Profunctor
import Data.Semigroupoid
import Prelude hiding ((.), and, id, last, any, null, maximum, minimum, lookup, all, product, elem, or)


-- | `Fold` left or return an early result.
--
-- Normal `Fold`s are strict and thus loop on infinite input:
--
-- @
--  λ> F.fold F.head ([1..] :: [Int])
--  ^CInterrupted.
--
--  λ> foldE headE ([1..] :: [Int])
--  Just 1
--  (0.05 secs, 450,312 bytes)
-- @
--
-- Since `FoldE` returns early upon success, it can terminate
-- on infinite inputs.
--
-- Can be composed in at least three ways!
--
-- See the `Semigroupoid` instance for the first way: vertical composition.
newtype FoldE a b = FoldE { runFoldE :: FoldM (Either b) a b }

instance Profunctor FoldE where
  lmap = withFoldE . lmap
  rmap = fmap

instance Functor (FoldE a) where
  fmap f (FoldE g) = (`impurely` g) $ \stp ini ext ->
    FoldE $
      FoldM
        (\x -> first f . stp x)
        (first f ini)
        (join bimap f . ext)

instance Extend (FoldE a) where
  duplicated = withFoldE $ fmap FoldE . F.hoists (first return) . duplicated

instance Comonad (FoldE a) where
  extract = impurely (\_ ini ext -> joinEither $ ini >>= ext) . runFoldE

  duplicate = duplicated

instance Applicative (FoldE a) where
  pure x = FoldE $ FoldM (\_ _ -> Left x) (Left x) (\_ -> Left x)

  (<*>) = (<.>)

instance Apply (FoldE a) where
  fs <.> xs = foldEThenE fs (<$> xs)

instance Bind (FoldE a) where
  (>>-) = foldEThenERec

-- | We're using the faster `foldEThenERec`
instance Monad (FoldE a) where
  (>>=) :: FoldE a b -> (b -> FoldE a c) -> FoldE a c
  (>>=) = foldEThenERec

instance MonadFix (FoldE a) where
  mfix :: (b -> FoldE a b) -> FoldE a b
  mfix g =
    let f =
          (`impurely` runFoldE f) $ \stp ini ext ->
            FoldE $
            FoldM
              (\x y ->
                 either
                   (fmap Left . unwrapFoldE . stepFoldE y)
                   (Right . first g . (`stp` y))
                   x)
              (Right $ first g ini)
              (bimap extract (extract . g . joinEither . ext))
     in f

instance MonadZip (FoldE a) where
  mzipWith :: (b -> c -> d) -> FoldE a b -> FoldE a c -> FoldE a d
  mzipWith f (FoldE g) (FoldE h) =
    FoldE $
    (`impurely` g) $ \stpG iniG extG ->
      (`impurely` h) $ \stpH iniH extH ->
        FoldM
          (\(~(xG, xH)) ->
             liftM2
               (combineLefts f)
               ((xG >>=) . flip stpG)
               ((xH >>=) . flip stpH))
          (combineLefts f iniG iniH)
          (\(~(x, y)) ->
             Right $
             f
               (either id (joinEither . extG) x)
               (either id (joinEither . extH) y))

-- | Since `FoldE` can return before consuming the entire input, we can repeat it on the input
-- and fold over its results using the composed `FoldE`.
instance Semigroupoid FoldE where
  o :: FoldE b c -> FoldE a b -> FoldE a c
  {-# INLINE o #-}
  o (FoldE f) (FoldE g) =
    (`impurely` f) $ \stpF iniF extF ->
      (`impurely` g) $ \stpG iniG extG ->
        let ~(Right iniGR) = iniG
         in FoldE $
            FoldM
              (\(~(xF, xG)) y ->
                 case stpG xG y of
                   Left xG' -> (, iniGR) <$> stpF xF xG'
                   ~(Right xG') -> Right (xF, xG'))
              (iniF >>=
               flip
                 (bimap . (either id (joinEither . extF) .) . stpF <*> (,))
                 iniG)
              (\(~(xF, xG)) ->
                 joinEither . extF <$> stpF xF ((joinEither . extG) xG))

-- | Unwrap and wrap the `FoldE` newtype
{-# INLINE withFoldE #-}
withFoldE :: (FoldM (Either b) a b -> FoldM (Either d) c d) -> FoldE a b -> FoldE c d
withFoldE f = FoldE . f . runFoldE

-- | TODO: compare performance to `fmap`
fmapAlternative :: (b -> c) -> FoldE a b -> FoldE a c
fmapAlternative f = withFoldE $ F.hoists (first f) . fmap f

-- | `stepFoldM`, specialized to `FoldE`
{-# INLINE stepFoldE #-}
stepFoldE :: a -> FoldE a b -> FoldE a b
stepFoldE x = FoldE . stepFoldM x . runFoldE

-- | `unwrapFoldM` specialized to `FoldE`
{-# INLINE unwrapFoldE #-}
unwrapFoldE :: FoldE a b -> Either b (FoldE a b)
unwrapFoldE = fmap FoldE . unwrapFoldM . runFoldE

-- | Take @n@ and return a list
--
-- @
--  λ> foldE (toFoldE F.list `o` takeE 3) [1..10]
--  [[1,2,3],[4,5,6],[7,8,9],[10]]
-- @
--
takeE :: Int -> FoldE a [a]
takeE n =
  FoldE $
  FoldM
    (\(i, prv) x ->
       if i < n - 1
         then return (i + 1, prv ++ [x])
         else Left $ prv ++ [x])
    (return (0, []))
    (return . snd)

-- | Drop @n@ and return nothing
--
-- @
--  λ> foldE (toFoldE F.list `o` (dropE 3 >> headE)) [1..10]
--  [Just 5,Just 9,Nothing]
-- @
--
dropE :: Int -> FoldE a ()
dropE n =
  FoldE $
  FoldM
    (\i _ ->
       if i < n
         then return (i + 1)
         else Left ())
    (Right 0)
    (const (Left ()))

-- | Take a single line (and toss its end, if it has one)
lineE :: FoldE Char String
lineE = takeWhileE (/= '\n')

-- | Return the first value and always fail if extracted earlier.
--
-- I'm not really sure why the values are being duplicated.. but it's probably because
-- the first early return is assumed to have _not_ consumed the last value it reached..
--
-- @
--  λ> foldE (toFoldE F.list `o` headE) [1..3]
--  [Just 1,Just 1,Just 2,Just 2,Just 3,Just 3,Nothing]
-- @
--
headE :: FoldE a (Maybe a)
headE =
  FoldE (FoldM (const $ Left . Just) (Right ()) (const $ Right Nothing))

-- | Drop the first element provided or return `()` if extracted.
{-# INLINE tailE #-}
tailE :: FoldE a ()
tailE = FoldE $ FoldM (\_ _ -> Left ()) (Right ()) (const (Left ()))

-- | Return `Just` the last element or `Nothing`
{-# INLINE lastE #-}
lastE :: FoldE a (Maybe a)
lastE = toFoldE F.last

-- | Count the number of elements in the prefix that satisfy the predicate.
-- Should be equivalent to:
-- @
--  `foldWhile` p `length`
-- @
{-# INLINE countPrefixE #-}
countPrefixE :: Enum i => (a -> Bool) -> FoldE a i
countPrefixE p =
  FoldE $
  FoldM
    (\x y ->
       if p y
         then return . (toEnum . (+ 1) . fromEnum) $ x
         else Left x)
    (Right (toEnum 0))
    Right

-- | Convert from a `FoldE` to a `Fold` by binding (`>>=`)
-- steps together and using `joinEither` on the result.
fromFoldE :: FoldE a b -> Fold a b
fromFoldE (FoldE f) =
  (`impurely` f) $ \stp ini ext ->
    Fold (\x y -> x >>= (`stp` y)) ini (either id (joinEither . ext))

-- | Run a `FoldE` over a `Foldable`
{-# SPECIALIZE foldE :: FoldE a b -> [a] -> b #-}
{-# INLINE foldE #-}
foldE :: Foldable t => FoldE a b -> t a -> b
foldE = fmap joinEither . F.foldM . runFoldE

-- | Never returns `Left`. `fromFoldE . toFoldE == id`
{-# INLINE toFoldE #-}
toFoldE :: Fold a b -> FoldE a b
toFoldE = FoldE . F.generalize

-- | `foldE1` specialized to `Identity` and pre-applied for convenience.
{-# INLINE foldE1 #-}
foldE1 :: FoldE a b -> a -> b
foldE1 = lmap Identity . foldE

-- | Fold with the first fold until it returns `Left`,
-- then feed its input into the second fold and continue.
{-# INLINE foldEThenE #-}
foldEThenE :: FoldE a b -> (b -> FoldE a c) -> FoldE a c
foldEThenE (FoldE f) g =
  (`impurely` f) $ \stp ini ext ->
    FoldE $
    FoldM
      (\x y ->
         either
           (fmap Left . unwrapFoldE . stepFoldE y)
           (Right . first (stepFoldE y . g) . (`stp` y))
           x)
      (Right $ first g ini)
      (bimap extract (extract . g . joinEither . ext))

-- | Unfold the successive applications of a `FoldE` into a `Cofree` `ReaderT`
toCofreeReader :: FoldE a b -> Cofree (ReaderT a (Either b)) b
toCofreeReader (FoldE f) =
  (`impurely` f) $ \stp ini ext ->
    let step' =
          liftM2 (:<) (joinEither . ext) (ReaderT . fmap (fmap step') . stp)
     in either (liftM2 (:<) id (ReaderT . const . Left)) step' ini

-- | Convert a `Cofree` `ReaderT` to a `FoldE`
fromCofreeReader :: Either b (Cofree (ReaderT a (Either b)) b) -> FoldE a b
fromCofreeReader xs = FoldE $ FoldM (runReaderT . unwrap) xs (Right . extract)

-- | Convert a `FoldE` to a `FreeFoldM` using `toCofreeReader`
{-# INLINE foldEToFree #-}
foldEToFree :: FoldE a b -> FreeFoldM (Either b) a b
foldEToFree = FreeFoldM . return . toCofreeReader

-- | Convert a `FreeFoldM` to a `FoldE` using `fromCofreeReader`
freeToFoldE :: FreeFoldM (Either b) a b -> FoldE a b
freeToFoldE = fromCofreeReader . runFreeFoldM

-- | Fold with the first `FoldE` until `Left`,
-- then use its result to construct the second fold and continue.
foldEThenFree :: FoldE a b -> (b -> FoldE a c) -> FreeFoldM (Either c) a c
foldEThenFree (FoldE f) g =
  (`impurely` f) $ \stp ini ext ->
    let extract' = extract . g . joinEither . ext -- :: x -> c
        step' = liftM2 (:<) extract' (ReaderT . fmap stepEither' . stp) -- :: x -> Cofree (ReaderT a (Either c)) c
        stepEither' = return . either (toCofreeReader . g) step' -- :: Either b x -> Either c (Cofree (ReaderT a (Either c)) c)’
     in FreeFoldM $ stepEither' ini

-- | Uses `foldFreeThenFree` instead of directly recursing
{-# INLINE foldEThenFree' #-}
foldEThenFree' :: FoldE a b -> (b -> FoldE a c) -> FreeFoldM (Either c) a c
foldEThenFree' f g = foldFreeThenFree (foldEToFree f) (foldEToFree . g)

-- | So, I was afraid that this wouldn't be more performant, but here it is:
--
-- @
-- λ> (foldE $ foldEThenE (takeE 3) (\x -> (length x +) <$> toFoldE F.sum)) $ [1..2^20]
-- (2.77 secs, 1,771,691,168 bytes)
--
-- λ> (foldE $ foldEThenE (takeE 3) (\x -> (length x +) <$> toFoldE F.sum)) $ [1..2^21]
-- (5.41 secs, 3,542,965,480 bytes)
--
-- λ> (foldE $ foldEThenERec (takeE 3) (\x -> (length x +) <$> toFoldE F.sum)) $ [1..2^20]
-- (0.68 secs, 857,335,424 bytes)
--
-- λ> (foldE $ foldEThenERec (takeE 3) (\x -> (length x +) <$> toFoldE F.sum)) $ [1..2^21]
-- (1.50 secs, 1,714,251,384 bytes)
-- @
--
-- Of course we'll have to wait for criterion benchmarks to really tell.
{-# INLINE foldEThenERec #-}
foldEThenERec :: FoldE a b -> (b -> FoldE a c) -> FoldE a c
foldEThenERec f = FoldE . freeFoldM . foldEThenFree f

-- | `foldEThenERec`, using `foldFreeThenFree` instead of directly recursing.
--
-- Preliminary tests show it may be a hair faster than `foldEThenERec`,
-- but we'll have to wait for criterion tests to really tell.
{-# INLINE foldEThenERec' #-}
foldEThenERec' :: FoldE a b -> (b -> FoldE a c) -> FoldE a c
foldEThenERec' f = FoldE . freeFoldM . foldEThenFree' f

-- | `foldEThenE`, but drop the first result
foldEThenE_ :: FoldE a b -> FoldE a c -> FoldE a c
foldEThenE_ f = foldEThenE f . const

-- | `foldEThenE`, but never fail with the second fold
foldEThen :: FoldE a b -> (b -> Fold a c) -> Fold a c
foldEThen f = fromFoldE . foldEThenE f . (toFoldE .)

-- | `foldEThenE_`, but never fail on the second fold
foldEThen_ :: FoldE a b -> Fold a c -> Fold a c
foldEThen_ f = foldEThen f . const

-- | Ok, I think this is probably _not_ very efficient,
-- but here goes:
--
-- Example:
--
-- @
--  `peekLast` `(,)` :: `FoldE` a b -> `FoldE` a (`Either` b (a, b))
--  `peekLast` `(,)` `F.head` :: `FoldE` a (`Either` (`Maybe` a) (a, `Maybe` a))
--
--  λ> peekLast headE (,) `foldE` [1..1]
--  Right (1,Just 1)
--  (0.02 secs, 549,328 bytes)
--  λ> peekLast headE (,) `foldE` [1..10]
--  Right (1,Just 1)
--  (0.01 secs, 549,328 bytes)
-- @
--
peekLast :: FoldE a b -> (a -> b -> c) -> FoldE a (Either b c)
peekLast (FoldE f) g =
  (`impurely` f) $ \stp ini ext ->
    case ini of
      Left ini' -> pure $ Left ini'
      ~(Right ini') ->
        FoldE $
        FoldM
          (\x y -> (return . g y) `first` stp x y)
          (return ini')
          (bimap Left Left . ext)

-- | `peekLast`, discarding the result of the `FoldE`
peekLast_ :: FoldE a b -> (a -> c) -> FoldE a (Either b c)
peekLast_ f g = peekLast f (\x _ -> g x)

-- | -- We can use this with other combinators to build folding parsers.
--
-- Warning: composing two equivalent `takeWhileE`'s will diverge
{-# INLINE takeWhileE #-}
takeWhileE :: (a -> Bool) -> FoldE a [a]
takeWhileE = (`foldWhile` F.list)

-- | Fold until the predicate fails
{-# INLINE foldWhile #-}
foldWhile :: (a -> Bool) -> Fold a b -> FoldE a b
foldWhile p f =
  (`purely` f) $ \stp ini ext ->
    FoldE $
    FoldM
      (\x y ->
         if p y
           then Right $ stp x y
           else Left $ ext x)
      (return ini)
      (return . ext)

-- | Fold until the predicate succeeds
{-# INLINE foldUntil #-}
foldUntil :: (a -> Bool) -> Fold a b -> FoldE a b
foldUntil = foldWhile . fmap not

-- | Fold until the given fold returns early or the predicate fails
{-# INLINE foldWhileE #-}
foldWhileE :: (a -> Bool) -> FoldE a b -> FoldE a b
foldWhileE p (FoldE f) =
  (`impurely` f) $ \stp ini ext ->
    FoldE $
    FoldM
      (\x y ->
         if p y
           then stp x y
           else Left . joinEither $ ext x)
      ini
      ext

-- | Fold until the given fold returns early or the predicate succeeds
{-# INLINE foldUntilE #-}
foldUntilE :: (a -> Bool) -> FoldE a b -> FoldE a b
foldUntilE = foldWhileE . fmap not

-- | If there's no previous element, equivalent to `foldWhile` on the first predicate.
-- If there's a previous element, then apply to it and the current element:
--
-- @
--  predicate2 previous current
-- @
--
foldWhile2 :: (a -> Bool) -> (a -> a -> Bool) -> Fold a b -> FoldE a b
foldWhile2 p1 p2 =
  fromCofreeReader .
  Right . foldWhileFree2 p1 p2 . runIdentity . runFreeFoldM . toFreeFold

-- | foldWhile2 implemented using @Cofree (ReaderT a (Either b)) b@
foldWhileFree2 ::
     (a -> Bool)
  -> (a -> a -> Bool)
  -> Cofree (Reader a) b
  -> Cofree (ReaderT a (Either b)) b
foldWhileFree2 p1 p2 ~(x :< ReaderT xs) =
  x :<
  ReaderT
    (\y ->
       if p1 y
         then Right . foldWhileFree2' p2 y . runIdentity $ xs y
         else Left x)

-- | `foldWhileFree2`, given an initial value
foldWhileFree2' ::
     (a -> a -> Bool)
  -> a
  -> Cofree (Reader a) b
  -> Cofree (ReaderT a (Either b)) b
foldWhileFree2' p x ~(y :< ReaderT ys) =
  y :<
  ReaderT
    (\z ->
       if p x z
         then Right . foldWhileFree2' p z . runIdentity $ ys z
         else Left y)


-- | If there's no previous element, equivalent to `foldWhile` on the first predicate.
-- If there's a previous element, then we do:
--
-- @
--  predicate1 previous && predicate2 current
-- @
--
-- I believe it can be more efficient than `foldWhile2`, but of course your
-- predicate has to be separable.
bifoldWhile :: (a -> Bool) -> (a -> Bool) -> Fold a b -> FoldE a b
bifoldWhile p1 p2 =
  fromCofreeReader .
  Right . bifoldWhileFree p1 p2 . runIdentity . runFreeFoldM . toFreeFold

-- | See `bifoldWhile`
bifoldWhileFree ::
     (a -> Bool)
  -> (a -> Bool)
  -> Cofree (Reader a) b
  -> Cofree (ReaderT a (Either b)) b
bifoldWhileFree p1 p2 ~(x :< ReaderT xs) =
  x :<
  ReaderT
    (\y ->
       if p1 y
         then Right . foldWhileFree p2 . runIdentity $ xs y
         else Left x)

-- | See `foldWhile`
foldWhileFree ::
     (a -> Bool)
  -> Cofree (Reader a) b
  -> Cofree (ReaderT a (Either b)) b
foldWhileFree p ~(x :< ReaderT xs) =
  x :<
  ReaderT
    (\y ->
       if p y
         then Right . foldWhileFree p . runIdentity $ xs y
         else Left x)


-- | `foldWhile2`, but the `FoldE` can return early
foldWhileE2 :: (a -> Bool) -> (a -> a -> Bool) -> FoldE a b -> FoldE a b
foldWhileE2 p1 p2 = fromCofreeReader . Right . foldWhileFreeE2 p1 p2 . toCofreeReader

-- | foldWhileE2 implemented using @Cofree (ReaderT a (Either b)) b@
foldWhileFreeE2 ::
     (a -> Bool)
  -> (a -> a -> Bool)
  -> Cofree (ReaderT a (Either b)) b
  -> Cofree (ReaderT a (Either b)) b
foldWhileFreeE2 p1 p2 ~(x :< ReaderT xs) = x :< ReaderT (\y -> if p1 y then foldWhileFreeE2' p2 y <$> xs y else Left x)

-- | `foldWhileFreeE2`, given an initial value
foldWhileFreeE2' :: (a -> a -> Bool) -> a -> Cofree (ReaderT a (Either b)) b -> Cofree (ReaderT a (Either b)) b
foldWhileFreeE2' p x ~(y :< ReaderT ys) = y :< ReaderT (\z -> if p x z then foldWhileFreeE2' p z <$> ys z else Left y)


-- | `bifoldWhile`, but the `FoldE` can return early
bifoldWhileE :: (a -> Bool) -> (a -> Bool) -> FoldE a b -> FoldE a b
bifoldWhileE p1 p2 (FoldE f) = (`impurely` f) $ \stp ini ext -> runIdentity $ do
  let folder Nothing y = if p1 y
                            then ini >>= fmap Just . (`stp` y)
                            else ini >>= Left . joinEither . ext
      folder ~(Just x) y = if p2 y
                              then Just <$> stp x y
                              else Left . joinEither $ ext x
  return . FoldE $ FoldM folder (return Nothing) (maybe (ini >>= ext) ext)

-- | `bifoldWhileE` implemented in terms of @Cofree (ReaderT a (Either b)) b@.
bifoldWhileE' :: (a -> Bool) -> (a -> Bool) -> FoldE a b -> FoldE a b
bifoldWhileE' p1 p2 = fromCofreeReader . Right . bifoldWhileFreeE p1 p2 . toCofreeReader

-- | Since we can map over a single layer of a `Cofree`,
-- returning `Left` if the first predicate fails, we can apply
-- the second predicate using `foldWhileFreeE`.
--
-- This avoids the need for `Maybe` in the state variable.
bifoldWhileFreeE ::
     (a -> Bool)
  -> (a -> Bool)
  -> Cofree (ReaderT a (Either b)) b
  -> Cofree (ReaderT a (Either b)) b
bifoldWhileFreeE p1 p2 ~(x :< ReaderT xs) =
  x :<
  ReaderT
    (\y ->
       if p1 y
         then foldWhileFreeE p2 <$> xs y
         else Left x)

-- | See `foldWhileE`
foldWhileFreeE ::
     (a -> Bool)
  -> Cofree (ReaderT a (Either b)) b
  -> Cofree (ReaderT a (Either b)) b
foldWhileFreeE p ~(x :< ReaderT xs) =
  x :<
  ReaderT
    (\y ->
       if p y
         then foldWhileFreeE p <$> xs y
         else Left x)

-- | Apply the function, ignoring the `Nothing`s
prefilterMaybe :: (a -> Maybe b) -> FoldE b c -> FoldE a c
prefilterMaybe f (FoldE g) = (`impurely` g) $ \stp ini ext -> FoldE $ FoldM (\x y -> maybe (return x) (stp x) (f y)) ini ext



-- | Skip while the predicate holds.
--
-- I'm not sure whether this should have one of the types:
--
-- @
--  FoldE a a
--  FoldE a (Maybe a)
-- @
--
-- To allow it to return the element that the predicate fails on.
--
-- Should be equivalent to:
--
-- @
--  void . foldWhile (pure ())
-- @
--
{-# INLINE skipWhile #-}
skipWhile :: (a -> Bool) -> FoldE a ()
skipWhile p =
  FoldE $
  FoldM
    (\_ y -> p y `unless` Left ())
    (Right ())
    (const $ Right ())

-- | Skip until the predicate holds
{-# INLINE skipUntil #-}
skipUntil :: (a -> Bool) -> FoldE a ()
skipUntil = skipWhile . fmap not

-- | If the predicate passes, return the value.
-- If folded over nothing, return `Nothing`.
satisfyE :: (a -> Bool) -> FoldE a (Maybe a)
satisfyE p =
  FoldE $
  FoldM
    (\_ y ->
       Left $
       if p y
         then Just y
         else Nothing)
    (Right ())
    (const (Left Nothing))

-- | Are there any elements?
null :: FoldE a Bool
null = FoldE $ FoldM (\_ _ -> Left False) (return ()) (\_ -> return True)

-- | Returns True if all elements are True, False otherwise
and :: FoldE Bool Bool
and =
  FoldE $ FoldM (\_ -> (`unless` Left False)) (return ()) (\_ -> return True)

-- | Returns True if any elements are True, False otherwise
or :: FoldE Bool Bool
or = FoldE $ FoldM (\_ -> (`when` Left True)) (return ()) (\_ -> return False)

-- | Returns True if all elements satisfy the predicate, False otherwise
all :: (a -> Bool) -> FoldE a Bool
all p =
  FoldE $
  FoldM (\_ -> (`unless` Left False) . p) (return ()) (\_ -> return True)

-- | Returns True if any elements satisfy the predicate, False otherwise
any :: (a -> Bool) -> FoldE a Bool
any p =
  FoldE $ FoldM (\_ -> (`when` Left True) . p) (return ()) (\_ -> return False)

-- | Returns early on `0`.
--
-- Use `toFoldE` with the `F.product` from @Control.Foldl@
-- to multiply all elements.
--
-- Note: will likely be less performant if you're unlikely to
-- encounter @0@.
product :: (Num a, Eq a) => FoldE a a
product = foldUntil (/= 0) F.product

-- | Is the given value an element?
--
-- This is preetty slow.. upwards of @30x@ slower than `Data.Foldable.elem`
elem :: Eq a => a -> FoldE a Bool
elem x = fromRight False <$> peekLast_ (skipUntil (== x)) (== x)

-- | Wow, this is still pretty slow.
-- (It's about @70%@ faster with lists of around @2^20@ elements).
-- Maybe if it's inlined and -O3'd?
elem' :: Eq a => a -> FoldE a Bool
elem' x = FoldE $ FoldM (\_ -> (`when` Left True) . (== x)) (return ()) (\_ -> return False)

-- | Either return `Just` the first value matching the predicate or `Nothing`.
find :: (a -> Bool) -> FoldE a (Maybe a)
find p = do
  skipUntil p
  headE

-- | Either return `Just` the element at the given index
-- (`0` at the beginning of the `FoldE`) or `Nothing`.
index :: Int -> FoldE a (Maybe a)
index i =
  FoldE $
  FoldM
    (\j y ->
       if i == j
         then Left (Just y)
         else Right (j + 1))
    (Right 0)
    (const $ Right Nothing)

-- | Return `Just` the value associated with the given key or `Nothing`
lookup :: Eq a => a -> FoldE (a, b) (Maybe b)
lookup x =
  FoldE $
  FoldM
    (\_ ~(y, z) ->
       if x == y
         then Left (Just z)
         else Right ())
    (Right ())
    (const $ Right Nothing)

-- | Returns early on `minBound`.
--
-- Use `toFoldE` with the `F.minimum` for
-- the general case.
--
-- Likely to be much less performant if you don't expect to have many
-- `minBound`s in what you're folding.
minimum :: (Bounded a, Ord a) => FoldE a (Maybe a)
minimum = foldUntil (== minBound) F.minimum

-- | Returns early on `maxBound`.
--
-- Likely to be much less performant if you don't expect to have many
-- `maxBound`s in what you're folding.
maximum :: (Bounded a, Ord a) => FoldE a (Maybe a)
maximum = foldUntil (== maxBound) F.maximum

-- | Assumes the input keys are monotonically increasing (`inc`)
lookupOrd :: Ord a => a -> FoldE (a, b) (Maybe b)
lookupOrd x = FoldE $ FoldM (const stp) (Right ()) (const $ Right Nothing)
  where
    stp ~(y, z) =
      case x `compare` y of
        LT -> Right ()
        EQ -> Left $ Just z
        GT -> Left Nothing

-- | `Just` the first index of the given value or `Nothing`
elemIndex :: Eq a => a -> FoldE a (Maybe Int)
elemIndex x =
  FoldE $
  FoldM
    (\i y ->
       if x == y
         then Left (Just i)
         else Right (i + 1))
    (Right 0)
    (const $ Right Nothing)

-- | Assumes the inputs are monotonically increasing (`inc`)
elemIndexOrd :: Ord a => a -> FoldE a (Maybe Int)
elemIndexOrd x = FoldE $ FoldM stp (Right 0) (const $ Right Nothing)
  where
    stp i y =
      case x `compare` y of
        LT -> Right (i + 1)
        EQ -> Left $ Just i
        GT -> Left Nothing

-- | Find the first index for which the predicate returns `True`
findIndex :: (a -> Bool) -> FoldE a (Maybe Int)
findIndex p =
  FoldE $
  FoldM
    (\i x ->
       if p x
         then Left (Just i)
         else Right (i + 1))
    (Right 0)
    (const $ Right Nothing)

-- | `index` for any `Integral`
genericIndex :: Integral i => i -> FoldE a (Maybe a)
genericIndex i =
  FoldE $
  FoldM
    (\j y ->
       if i == j
         then Left (Just y)
         else Right (j + 1))
    (Right 0)
    (const $ Right Nothing)

-- | Are all elements equal?
eq :: Eq a => FoldE a Bool
eq = do
  head' <- headE
  flip (maybe $ return True) head' $ \x -> do
    skipWhile (/= x)
    lastE' <- lastE
    return $ maybe True (== x) lastE'

-- | Monotonically increasing
inc :: Ord a => FoldE a Bool
inc = foldE2 (const True) (<=) and

-- | Strictly increasing
inc' :: Ord a => FoldE a Bool
inc' = foldE2 (const True) (<) and

-- | Monotonically decreasing
dec :: Ord a => FoldE a Bool
dec = foldE2 (const True) (>=) and

-- | Strictly decreasing
dec' :: Ord a => FoldE a Bool
dec' = foldE2 (const True) (>) and

-- | Given a fold over the results of cases for one or and maybe the previous
-- element, produce a fold over arbitrary inputs.
--
-- @
--   diffs :: Num a => FoldE a (Maybe a)
--   diffs = foldE2 ifOne ifMultiple _
--    where
--      ifOne = Just
--      ifMultiple = liftM2 (flip (-))
-- @
--
-- I've found it quite helpful for folding over pairs of elements.
foldE2 :: (a -> b) -> (a -> a -> b) -> FoldE b c -> FoldE a c
foldE2 f1 f2 (FoldE gs) =
  (`impurely` gs) $ \stp ini ext ->
    FoldE $
    FoldM
      (\x y ->
         return . (, y) <$> either (`stp` f1 y) (uncurry stp . fmap (`f2` y)) x)
      (Left <$> ini)
      (ext . either id fst)

-- | `foldE2` using `foldFree2`
{-# INLINE foldERec2 #-}
foldERec2 :: (a -> b) -> (a -> a -> b) -> FoldE b c -> FoldE a c
foldERec2 f1 f2 = FoldE . freeFoldM . foldFree2 f1 f2 . foldEToFree

