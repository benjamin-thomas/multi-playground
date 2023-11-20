import Data.Char (toUpper)

{-
   A functor is a typeclass that represents things that can be mapped over.
   It's defined by the `Functor` typeclass, which has a single method: `fmap`.

   Examples of types being functors:

     - List
     - Maybe
-}
data MyBox a = MyBox a
  deriving Show

instance Functor MyBox where
  fmap f (MyBox x) = MyBox (f x)

{- | Now, we can map over our custom type. Note that the type may be inferred from
the start or the end of the expression.

>>> fmap (*2) (MyBox 3 :: MyBox Int)
MyBox 6
>>> fmap (*2) (MyBox 4) :: MyBox Int
MyBox 8

>>> fmap (*2) (MyBox 5 :: MyBox Float)
MyBox 10.0
>>> fmap (*2) (MyBox 6) :: MyBox Float
MyBox 12.0

>>> map toUpper "hello"
"HELLO"
>>> fmap (map toUpper) (MyBox "hello" :: MyBox String)
MyBox "HELLO"
>>> fmap (map toUpper) (MyBox "world") :: MyBox String
MyBox "WORLD"
-}

{- | We can also use the fmap operator <$>

>>> (*2) <$> (MyBox 7 :: MyBox Int)
MyBox 14

>>> (*2) <$> (MyBox 8) :: MyBox Int
MyBox 16

>>> (*2) <$> MyBox 9 :: MyBox Int
MyBox 18

>>> map toUpper <$> MyBox "hey" :: MyBox String
MyBox "HEY"
>>> map toUpper <$> MyBox "wow"
MyBox "WOW"
-}

{- |

The Functor typeclass has two "laws":

1) The identity law:

  fmap id = id

If we map the identity function over a functor, the resulting functor should be
the same!

2) The composition law:

  (fmap f) . (fmap g) = fmap (f . g)

Composing two functors with f and g applied to them respectively should be the
same as composing f and g and map once.

NOTE: function application goes from right to left.

>>> fmap (+1) . fmap (*2) $ [1,2,3]
[3,5,7]
>>> fmap ((+1) . (*2)) $ [1,2,3]
[3,5,7]

>>> fmap (*2) . fmap (+1) $ [1,2,3]
[4,6,8]
>>> fmap ((*2) . (+1)) $ [1,2,3]
[4,6,8]
-}
