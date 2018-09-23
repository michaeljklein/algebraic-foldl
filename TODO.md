
Missing utils:

```haskell
innerJoinFoldM :: FoldM m a (m b) -> FoldM m a b -- actually an iso
innerJoinFoldE :: FoldE a (Either b b) -> FoldE a b
innerJoinFoldEM :: FoldEM m a (ExceptT b m b) -> FoldEM m a b

wrapFoldM :: m (FoldM m a b) -> FoldM m a b

m (x -> a -> m x, m x, x -> m b) -> (x -> a -> m x, m x, x -> m b)

m (x -> a -> m x, m x, x -> m b) -> (x -> a -> m x, m x, x -> m b)

fmap fst3 :: m (x -> a -> m x, _, _) -> m (x -> a -> m x)
fmap fst3 :: m (x -> a -> m x) -> (x -> a -> m x)
fmap (fmap join . distribute) . distribute

(>>= snd3) :: m (_, m x, _) -> m x

fmap join . distribute . fmap trd3 :: m (_, _, x -> m b) -> (x -> m b)

```

- Should `FoldEM` be `FoldME`?
- Rename all `_E`'s to `_`
- Rename FreeFoldM to FoldFreeM or even FoldFreeT, if we can figure out the necessary instances
- Move the keys out of Control.Foldl.End
- Add quickcheck tests (mostly to prove equivalence with known methods)
- Add criterion benchmarks, compare to `foldl`
- Add `FoldECont` and `FoldEMCont` + tests + benchmarks comparing to regular
- Add `FoldEMany` and tests + benchmarks comparing to emitting lists, direct composition

Most need:

- instances
- folds
- conversion utils
- composition
- combinators

### Modules:

- Control.Foldl.NonEmpty

- Control.Foldl.Monadic.End

- Control.Foldl.Monadic.End.NonEmpty

- Control.Foldl.Monadic.NonEmpty

- Control.Foldl.End.Kleisli

- Control.Monad.Rep

- Data.Foldable.Free

  * FoldKey
  * Representable instance
  * `Fold a x -> x`
  * conversion (e.g. `fromFoldable :: Foldable t => t a -> FoldableFree a`)
  * instances, e.g. Foldable

- Data.Foldable.Monadic

```haskell
-- | Fold over containers with monadic context, e.g. `IOVar`s
-- (or maybe wrapped in a functor application?)
class Functor t => FoldableM m t | t -> m where
  foldrM :: (a -> b -> m b) -> b -> t a -> m b
  foldlM :: (b -> a -> m b) -> b -> t a -> m b
```

- Data.Foldable.Monadic.Free

  * FoldMKey
  * RepresentableM instance
  * `Fold a x -> x`
  * FoldableM instance

