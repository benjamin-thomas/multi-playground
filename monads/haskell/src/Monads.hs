{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Monads where

-------------------------------------------------------------------------------
-- VERBOSE STYLE
-------------------------------------------------------------------------------

{- | Adding 2 (maybe) numbers, verbose style.

There are 2 problems with this function:

1. the more params we add, the more it will indent to the right
2. repetition with the handling of "Nothing".

>>> add (Just 1) (Just 2)
Just 3

>>> add Nothing (Just 2)
Nothing
-}
add :: Maybe Int -> Maybe Int -> Maybe Int
add ma mb =
    case ma of
        Nothing -> Nothing
        Just a -> case mb of
            Nothing -> Nothing
            Just b -> Just (a + b)

-------------------------------------------------------------------------------
-- HANDLE THE COMMON FAILURE CASE
-------------------------------------------------------------------------------

-- Let's define a function to encapsulate this behavior
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen m fn = case m of
    Nothing -> Nothing
    Just a -> fn a

{- | Adding 2 (maybe) numbers, with `andThen`.

This version solves the repetition associated with handling the failure case, but not the indentation problem.

>>> add2 (Just 1) (Just 2)
Just 3

>>> add2 Nothing (Just 2)
Nothing
-}

{- FOURMOLU_DISABLE -}
add2 :: Maybe Int -> Maybe Int -> Maybe Int
add2 ma mb =
    andThen ma (\a ->
        andThen mb (\b ->
            Just (a + b)
        )
    )
{- FOURMOLU_ENABLE -}

-------------------------------------------------------------------------------
-- HANDLE THE COMMON FAILURE CASE + THE NESTING PROBLEM
-------------------------------------------------------------------------------

-- Let's use an operator to solve the indentation problem (normally this would be `>>=`)
(==>) :: Maybe a -> (a -> Maybe b) -> Maybe b
(==>) = andThen

{- | Still adding 2 (maybe) numbers, with `==>` now.

This version solves the repetition associated with handling the failure case, AND the indentation problem.

>>> add3 (Just 1) (Just 2)
Just 3

>>> add3 Nothing (Just 2)
Nothing
-}

{- FOURMOLU_DISABLE -}
add3 :: Maybe Int -> Maybe Int -> Maybe Int
add3 ma mb =
    ma ==> \a ->
    mb ==> \b ->
    Just (a + b)
{- FOURMOLU_ENABLE -}

-------------------------------------------------------------------------------
-- NOW FOR THE "REAL", MORE GENERAL CONCEPT (MONAD)
-------------------------------------------------------------------------------

{- | Adding 2 (wrapped) numbers, with the bind operator.
     The numbers themselves could be wrapped with `Maybe`, `Either`, etc.

This version solves the indentation problem + the repetition associated with handling the failure case.

Example for success:

>>> add4 (Just 1) (Just 2)
Just 3

>>> add4 (Right 1) (Right 2)
Right 3

---

Examples for failure:

(Reminder: `Right` represents success, `Left` represents failure)

>>> add4 Nothing (Just 2)
Nothing

>>> add4 (Just 1) Nothing
Nothing

>>> add4 (Left "could not compute first number!") (Right 2)
Left "could not compute first number!"

>>> add4 (Right 1) (Left "could not compute second number!")
Left "could not compute second number!"
-}

{- FOURMOLU_DISABLE -}
add4 :: (Monad m, Num n) => m n -> m n -> m n
add4 ma mb =
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