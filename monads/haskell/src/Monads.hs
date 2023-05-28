module Monads where

{- | Adding 2 (wrapped) numbers, with the bind operator.
     The numbers themselves could be wrapped with `Maybe`, `Either`, etc.

Example for success:

>>> add (Just 1) (Just 2)
Just 3

>>> add (Right 1) (Right 2)
Right 3

---

Examples for failure:

(Reminder: `Right` represents success, `Left` represents failure)

>>> add Nothing (Just 2)
Nothing

>>> add (Just 1) Nothing
Nothing

>>> add (Left "could not compute first number!") (Right 2)
Left "could not compute first number!"

>>> add (Right 1) (Left "could not compute second number!")
Left "could not compute second number!"
-}

{- FOURMOLU_DISABLE -}
add :: (Monad m, Num n) => m n -> m n -> m n
add ma mb =
    ma >>= \a ->
    mb >>= \b ->
    return (a + b)
{- FOURMOLU_ENABLE -}

-------------------------------------------------------------------------------

{- | Multiplying 2 (wrapped) numbers, with the do syntax.
     The numbers themselves could be wrapped with `Maybe`, `Either`, etc.

Examples:

>>> mul (Just 3) (Just 4)
Just 12

>>> mul Nothing (Just 4)
Nothing

>>> mul (Just 3) Nothing
Nothing
-}
mul :: (Monad m, Num n) => m n -> m n -> m n
mul ma mb = do
    a <- ma
    b <- mb
    return (a * b)