module Monads
  ( add
  , add_verbose
  , mul
  ) where

import Prelude

import Data.Maybe (Maybe(..))

add_verbose ∷ ∀ (n ∷ Type). Semiring n ⇒ Maybe n → Maybe n → Maybe n
add_verbose ma mb =
  case ma of
    Nothing -> Nothing
    Just a -> case mb of
      Nothing -> Nothing
      Just b -> Just (a + b)

add :: forall m n. Bind m => Applicative m => Semiring n => m n -> m n -> m n
add ma mb =
  ma >>= \a ->
    mb >>= \b ->
      pure (a + b)

mul :: forall m n. Bind m => Applicative m => Semiring n => m n -> m n -> m n
mul ma mb = do
  a <- ma
  b <- mb
  pure (a * b)
