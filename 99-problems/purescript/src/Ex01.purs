{-

https://ocaml.org/exercises#1

Write a function last : 'a list -> 'a option that returns the last element of a list

# last ["a" ; "b" ; "c" ; "d"];;
- : string option = Some "d"
# last [];;
- : 'a option = None

 -}

module Ex01
  ( last
  )
  where

import Data.Maybe (Maybe(..))
import Data.List hiding (last)

last :: forall a. List a -> Maybe a
last lst = case lst of
             Nil -> Nothing
             Cons x Nil -> Just x
             Cons _ xs -> last xs