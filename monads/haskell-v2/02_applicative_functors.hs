{-
  An applicative (functor) allows to apply a wrapped function to a wrapped value.
  All applicative functors are functors.

  An applicative defines `pure` and the apply operator (`<*>`).

---

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

---

  The `pure` function describes how to wrap the parameter.
  The apply operator `<*>` allows us to **chain operations** by wrapping a
  function in our structure.

  The name applicative comes from the fact that we "apply" functions from within
  the structure, rather than from outside as in normal functions.

---

instance  Applicative Maybe where
  pure = Just
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = (Just f x)

---

We can "lift" any value to become a `Maybe` value using `Just`, so this is the
implementation for `pure`.

For the function application, if either side is `Nothing`, our result is
`Nothing`. Otherwise, we apply the wrapped function to the wrapped value.
-}

{- |
Many basic functors are also applicatives.

For instance Maybe is an applicative functor:

>>> Just (+1) <*> Just 2
Just 3

>>> Nothing <*> Just 2
Nothing

>>> Just (+1) <*> Nothing
Nothing

>>> pure (+) <*> Just 1 <*> Just 2
Just 3

List is an applicative functor:

>>> pure (+) <*> [] <*> [1,2,3]
[]

>>> pure (+) <*> [0] <*> [1,2,3]
[1,2,3]

>>> pure (+) <*> [1] <*> [1,2,3]
[2,3,4]

>>> pure (+) <*> [0,1] <*> [1,2,3]
[1,2,3,2,3,4]

>>> pure (+) <*> [0,1,2] <*> [1,2,3]
[1,2,3,2,3,4,3,4,5]

>>> (+) <$> [0,1,2] <*> [1,2,3]
[1,2,3,2,3,4,3,4,5]

>>> pure (\a b c -> a + b + c) <*> [0] <*> [1,1,1,1] <*> [100]
[101,101,101,101]

>>> pure (\a b c -> a + b + c) <*> [0,0] <*> [1,1,1,1] <*> [100]
[101,101,101,101,101,101,101,101]

>>> pure (\a b c -> a + b + c) <*> [0] <*> [1,1,2,2] <*> [100]
[101,101,102,102]

>>> pure (\a b c -> a + b + c) <*> [0, 1] <*> [1,1,1,1] <*> [100]
[101,101,101,101,102,102,102,102]
-}

{- |
In short, applicatives help use to apply functions to wrapper types.
For instance, we can't add 2 maybes:

(Just 1) + (Just 2) <-- that's not going to work!

But we can do this:

>>> pure (\a b -> a + b) <*> (Just 1) <*> (Just 2)
Just 3

Or shorter:
>>> pure (+) <*> (Just 1) <*> (Just 2)
Just 3

Or even shorter:
>>> (+) <$> (Just 1) <*> (Just 2)
Just 3

>>> (*) <$> (Just 3) <*> (Just 4)
Just 12
-}


{-
Applicative have four laws:

-- Identity
pure id <*> v = v

-- Homomorphism
pure f <*> pure x = pure (f x)

-- Interchange
u <*> pure y = pure ($ y) <*> u

-- Composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-}
