{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-| This module provides a class for `FoldableM`, restricted to
    data-types that are non-empty up to the `Monad` being non-empty.
-}

module Data.Foldable.Monadic.NonEmpty where

import Control.Foldl.Monadic.NonEmpty (FoldM1(..), fromFoldM1)
import Control.Foldl.NonEmpty (Fold1(..), fromFold1)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable.Monadic (FoldableM(..))
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.Functor.NonEmpty (NonEmptyT(..))


-- | `FoldableM` for non-empty containers
class FoldableM m t => FoldableM1 m t| t -> m where
  pureFold1 :: Fold1 a b -> t a -> m b
  impureFold1 :: FoldM1 m a b -> t a -> m b

-- | Return `empty` unless the container being folded over is non-empty.
-- Otherwise behaves like `pureFold`.
pureFold1MaybeT :: FoldableM m t => Fold1 a b -> t a -> MaybeT m b
pureFold1MaybeT = fmap MaybeT <$> pureFold . fromFold1

-- | Return `empty` unless the container being folded over is non-empty.
-- Otherwise behaves like `impureFold`.
impureFold1MaybeT :: FoldableM m t => FoldM1 m a b -> t a -> MaybeT m b
impureFold1MaybeT = fmap MaybeT <$> impureFold . fromFoldM1

-- TODO:
--
-- instance FoldableM1 Identity NonEmpty
--
-- instance FoldableM m f => FoldableM1 m (NonEmptyT f)
--
-- instance FoldableM m f => FoldableM1 m (Cofree f)
--
-- instance FoldableM m f => FoldableM1 m (Free f)

