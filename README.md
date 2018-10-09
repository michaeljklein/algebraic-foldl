# algebraic-foldl

A library of experimental algebraic folds and combinators, building upon what's
provided in the [foldl](https://hackage.haskell.org/package/foldl) library.

## Control.Foldl.End

Folds with early results:

```haskell
newtype FoldE a b = FoldE { runFoldE :: FoldM (Either b) a b }
```


## Control.Foldl.NonEmpty

Folds over non-empty inputs:

```haskell
newtype Fold1 a b = Fold1
  { runFold1 :: ReaderT a (Fold a) b
  } deriving (Functor, Applicative)
```


## Data.Foldable.Free

A free `Foldable` container

```haskell
newtype FoldableFree a = FoldableFree
  { runFoldableFree :: forall x. Fold a x -> x
  }
```


## Control.Foldls

A `Fold` returning a stream of outputs.

```haskell
newtype Folds a b = Folds
  { runFolds :: Fold a (FoldableFree b)
  } deriving (Functor)
```


## Data.Distributive.Monadic

`Distributive` over a particular `Monad`:

```haskell
class Monad m => DistributiveM m f | f -> m where
  distributem :: m (f a) -> f (m a)

  collectm :: (a -> f b) -> m a -> f (m b)
```


## Control.Monad.Rep

`Representable` up to a particular `Monad`:

```haskell
class DistributiveM m f => RepresentableM m f | f -> m where
  type RepM f :: *

  tabulateM :: (RepM f -> m a) -> f a

  indexM :: f a -> (RepM f -> m a)
```


## Data.PseudoPoly

A pseudo-polymorphic data type:

```haskell
data PseudoPoly a b c where
  PseudoPoly :: a -> PseudoPoly a b b
```


# Docs

Haddock-generated documentation is available [here](https://michaeljklein.github.io/algebraic-foldl/)

