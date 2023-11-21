{-
A monad is a computational context.

Just as every applicative is a functor, every monad is also applicative.

--

class (Applicative m) => Monad m where
  return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

--

The bind operator is similar to the application operator in that it chains two
operations, with one of them being function related.

Let's compare the three operators seen so far:

fmap:
(<$>) :: (a -> b) -> f a -> f b

apply:
(<*>) :: f (a -> b) -> f a -> f b

flipped bind, to better show a common pattern:
(=<<) :: (a -> f b) -> f a -> f b

All these operators take a function, and then a wrapper type (producing another
wrapped type).

--

Just as `Maybe` is a functor, and an applictative functor, it's also a monad!

instance Monad Maybe where
  return    = Just
  Nothing >>= _ = Nothing
  Just a  >>= f = fa
-}

{- |
>>> Just 3 >>= \n -> Just 4 >>= \m -> Just (n * m)
Just 12

A cool example:
ghci> getLine >>= \input -> putStrLn $ map Data.Char.toUpper input
-}


{- Monads have three laws:

-- Left Identity
return x >>= f = f x

-- Right Identity
m >>= return m = m

-- Associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
-}
