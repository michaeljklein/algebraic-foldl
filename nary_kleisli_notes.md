
```haskell
-- | An `Ary` `Kleisli` category. Composition is provided by `replicateMApply`
newtype KleisliN (n :: N) (m :: * -> *) a b = KleisliN { runKleisliN :: Ary n a (m b) }

-- | Lift an `Ary` function returning a `Monad`ic value
-- to a `KleisliN`
liftAry :: Monad m => Ary n a b -> KleisliN n m a b
liftAry = KleisliN . fmap return

instance Monad m => Profunctor (KleisliN n m) where
  lmap f = KleisliN . lmap f . runKleisliN
  rmap = fmap

instance Monad m => Functor (KleisliN n m a) where
  fmap f = KleisliN . fmap (fmap f) . runKleisliN

instance (KnownN n, Monad m) => Applicative (KleisliN n m a) where
  pure = KleisliN . pure . pure

  KleisliN fs <*> KleisliN xs = KleisliN $ liftM2 (<*>) fs xs

instance (Monad m, KnownN n) => Monad (KleisliN n m a) where
  KleisliN xs >>= f = KleisliN $ xs >>= collect (undefined (runKleisliN . f))

instance Monad m => Category (KleisliN n m) where
  id :: KleisliN n m a a
  id = KleisliN $ undefined

  (.) :: KleisliN n m b c -> KleisliN n m a b -> KleisliN n m a c
  KleisliN f . KleisliN g = KleisliN $ join . replicateMApply f <$> g

instance Monad m => Arrow (KleisliN n m) where
  arr _ = undefined
  (***) = undefined



instance Monad m => ArrowM m (KleisliN n m) where
  arrM = undefined
  joinA = undefined
```


