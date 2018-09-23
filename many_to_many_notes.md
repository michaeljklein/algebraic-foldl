# Idea:

```haskell
-- | We use `Mapping` as a simpler representation of the isomorphic @CofreeMapping (->)@:
newtype Mapping a b = Mapping { runMapping :: a -> b }

-- | `CofreeMapping` is the natural representation of a one-to-one categorical mapping between types.
--
-- See: https://hackage.haskell.org/package/profunctors-5.3/docs/Data-Profunctor-Mapping.html#t:CofreeMapping
newtype CofreeMapping p a b = CofreeMapping { runCofreeMapping :: forall f. Functor f => p (f a) (f b) }

runIdentityP :: Profunctor p => p (Identity a) (Identity d) -> p a d
runIdentityP = dimap Identity runIdentity

toMapping :: CofreeMapping (->) a b -> Mapping a b
toMapping = Mapping . runIdentityP . runCofreeMapping

fromMapping :: Mapping a b -> CofreeMapping (->) a b
fromMapping = CofreeMapping . fmap . runMapping
```



| Variant     | From    | Allowed from size | To      | Allowed to size |
|-------------|---------|-------------------|---------|-----------------|
| Fold a b    | Many  a | 0 <= _            | One   b | 1 == _          |
| FoldE a b   | Many  a | 0 <= _            | Fewer b | 1 <= _ <= a     |
| Mapping a b | Many  a | 0 <= _            | Many  b | 0 <= _ == a     |
| Fold1 a b   | Some  a | 1 <= _            | One   b | 1 == _ <= a     |
| FoldE1 a b  | Some  a | 1 <= _            | Fewer b | 1 <= _ <= a     |

What about:

| Variant     | From    | Allowed from size | To      | Allowed to size |
|-------------|---------|-------------------|---------|-----------------|
| ???         | Many  a | 0 <= _            | More  b | a <= _          |

?

Well, one option is to use `[]` or `NonEmpty`:

```haskell
Fold a [b]
FoldE a [b]
Fold a (NonEmpty b)
FoldE a (NonEmpty b)
```

But that has some downsides, mostly from the interstitial data types.
(I suppose they could be lazily consumed, so we should benchmark to compare.)

Are they necessary? No.

One option is to return a function requiring a continuation:

```haskell
type FoldMany a b c = Fold a (Fold b c -> c)
type FoldManyE a b c = a (FoldE b c -> c)
```

Or we can weaken it by applying `distribute`:

```
distribute :: (Functor f, Distributive g) => f (g a) -> g (f a)
distribute :: (Functor f, Distributive ((->) (Fold b c)) => f (Fold b c -> c) -> Fold b c -> f c
distribute :: Fold a (Fold b c -> c) -> Fold b c -> Fold a c
distribute :: FoldMany a b c -> Fold b c -> Fold a c

distribute :: FoldManyE a b c -> FoldE b c -> FoldE a c
```

Can we derive an inverse, giving us an isomorphism?

Well the opposite of `Distributive` is `Traversable`:

```haskell
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```

But unfortunately `((->) a)` is not `Traversable`.

Let's unpack the types a bit and see if we can come up with something:

```haskell
toFoldMany :: (Fold b c -> Fold a c) -> FoldMany a b c
toFoldMany :: (Fold b c -> Fold a c) -> Fold a (Fold b c -> c)
toFoldMany :: (Fold b c -> Fold a c) -> (x3 -> a -> x3, x3, x3 -> Fold b c -> c)
toFoldMany :: (Fold b c -> (x2 -> a -> x2, x2, x2 -> c)) -> (x3 -> a -> x3, x3, x3 -> Fold b c -> c)
toFoldMany :: ((x1 -> b -> x1, x1, x1 -> c) -> (x2 -> a -> x2, x2, x2 -> c)) -> (x3 -> a -> x3, x3, x3 -> Fold b c -> c)
toFoldMany :: ((x1 -> b -> x1, x1, x1 -> c) -> (x2 -> a -> x2, x2, x2 -> c)) -> (x3 -> a -> x3, x3, x3 -> (x1 -> b -> x1, x1, x1 -> c) -> c)
toFoldMany :: ((x1 -> b -> x1) -> x1 -> (x1 -> c) -> (x2 -> a -> x2, x2, x2 -> c)) -> (x3 -> a -> x3, x3, x3 -> (x1 -> b -> x1, x1, x1 -> c) -> c)
toFoldMany :: ((x1 -> b -> x1) -> x1 -> (x1 -> c) -> (x2 -> a -> x2, x2, x2 -> c)) -> (x3 -> a -> x3, x3, x3 -> (x1 -> b -> x1) -> x1 -> (x1 -> c) -> c)
toFoldMany :: ((x1 -> b -> x1) -> x1 -> (x1 -> c) -> (x2 -> a -> x2, x2, x2 -> c)) -> (x2 -> a -> x2, x2, x2 -> (x1 -> b -> x1) -> x1 -> (x1 -> c) -> c)
```

Oh, we can just do:

```haskell
stepper :: a -> (Fold b c -> Fold a c) -> (Fold b c -> Fold a c)
stepper = fmap . stepFold

extractor :: (Fold b c -> Fold a c) -> (Fold b c -> c)
extractor = fmap extract

toFoldMany :: (Fold b c -> Fold a c) -> Fold a (Fold b c -> c)
toFoldMany f = Fold stepper f extractor
```

Boo ya.

Since this is based on `stepFold` and `extract`,
we can easily generalize it to `FoldM`, `FoldE`, etc.

Therefore, we have:

```haskell
Iso (FoldMany a b c) (Fold b c -> Fold a c)
Iso (FoldManyE a b c) (Fold b c -> Fold a c)
```

Note the obvious implementations:

```haskell
-- | Converts the first `Fold` `toFunction` and then
-- maps over the results of the second `Fold`.
pseudoCompose :: Fold b c -> Fold a b -> Fold a c
pseudoCompose = fmap . toFunction

mapFoldMany :: (a -> b) -> FoldMany a b c
mapFoldMany = toFoldMany . lmap

mapFoldManyE :: (a -> b) -> FoldMany a b c
mapFoldManyE = toFoldManyE . lmap

foldManyFromCompose :: Fold a b -> FoldMany a b c
foldManyFromCompose f = toFoldMany (. f)

foldManyFromComposeE :: FoldE a b -> FoldManyE a b c
foldManyFromComposeE f = toFoldManyE (. f)
```

One question is whether we can recover the `Fold`, `FoldE` from the `FoldMany`, `FoldManyE`, resp.

### Can we recover `Fold a b` from `forall x. FoldMany a b x`?

```haskell
-- | In this case, we can actually use `unsafeHead :: Fold a a`,
-- since we're guaranteed at least one output.
--
-- Hmm, but it seems that we may be simply taking the first result
-- from each non-empty "inner" group
fromFoldMany :: (forall x. FoldMany a b x) -> Fold a b
fromFoldMany :: (forall x. Fold b x -> Fold a x) -> Fold a b
fromFoldMany f = f unsafeHead

-- | Here, I believe that we may use `unsafeHead`, though
-- I'm not sure if any iterations are lost
fromFoldManyE :: (forall x. FoldManyE a b x) -> FoldE a b
fromFoldManyE :: (forall x. FoldE b x -> FoldE a x) -> FoldE a b
fromFoldManyE f = f unsafeHeadE

-- | For this case, we can just apply the function provided by the isomorphism
-- to `id`, since `FoldE1` has an identity, though I'm also not sure whether this one is valid.
fromFoldManyE1 :: (forall x. FoldManyE1 a b x) -> FoldE1 a b
fromFoldManyE1 :: (forall x. FoldE1 b x -> FoldE1 a x) -> FoldE1 a b
fromFoldManyE1 f = f id
```

So yes?

Therefore we have:

```haskell
forall a b. (forall c. FoldMany a b c `iso` Fold b c -> Fold a c) `iso` Fold a b
forall a b. (forall c. FoldManyE a b c `iso` FoldE b c -> FoldE a c) `iso` FoldE a b
```

## ALSO:

This is pretty side-tracked from where I was going, which was:

### Free Foldable!

```haskell
Fold a (FoldableFree b)
FoldE a (FoldableFree b)
```

Which is isomorphic to:

```haskell
Fold a (FoldMKey b)
FoldE a (FoldMKey b)
```

Now the question is, what's more composable:

```haskell
FoldManyEForall a b = FoldE a (forall c. FoldE b c -> c)
FoldManyEKeyed a b = FoldE a (FoldEKey b)
```

#### Note:
- Are these isomorphic?
- It doesn't look like it.


#### Note:
The default implementation of `FoldMKey` allows for empty keys,
but what if we want to ensure that each value is mapped to at least one?

We could do:

```haskell
Mapping a (NonEmpty b)
Mapping a (FoldE1 b c -> c)
Mapping a (b, FoldMKey b)
```

But all in all, that's proabably not what I'd call a "Fold";
it's more of a map or a non-empty `concatMap`.


## Cont.

I think that `forall x. Fold a x -> x` is equivalent to the free foldable
and `forall x. Fold m a x -> x` is the free monadic foldable, where:

```haskell
-- | This class allows us to fold over monadic containers, such as mutable 
-- vectors, `IOVar`s, and other container-like objects that we could fold
-- over if we were able to return a monadic result.
class (Monad m, Functor t) => FoldableM m t | t -> m where
  foldrM :: (a -> b -> m b) -> b -> t a -> m b

newtype WrappedFoldable t a = WrappedFoldable { runWrappedFoldable :: t a } deriving (Functor)

instance (Monad m, Foldable t) => FoldableM (WrappedFoldable t a) where
  foldrM = foldM

data SomeMVector v m a where
  SomeMVector :: MVector v a => m (v (PrimState m) a)

-- | Possible alternative to `SomeMVector`
data MVectorF v a m b where
  MVectorF :: forall x. MVector v x => m (v (PrimState m) x) -> (FoldM m x b -> b) -> MVectorF v m a b

-- ORRR
fromMVector :: (PrimMonad m, MVector v a) => v (PrimState m) a -> FoldableFreeM m a


instance PrimMonad m => Foldable m (SomeMVector v m)

newtype FoldableFree a = FoldableFree { runFoldableFree :: forall x. Fold a x -> x }

instance Functor FoldableFree where
  fmap f (FoldableFree x) = FoldableFree (lmap (lmap f) x)

instance Foldable FoldableFree where
  foldlM f x xs = xs `runFoldableFree` Fold f x id

newtype FoldableFreeM m a = FoldableFreeM { runFoldableFreeM :: forall x. FoldM m a x -> m x }

instance Monad m => FoldableM (FoldableFreeM m) where
  foldlM f x xs = xs` runFoldableFreeM` FoldM f x return
```

