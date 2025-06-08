-- import Data.List (unfoldr)

{- | Naive version

>>> fib 10
55

*Main> fib 25
75025
(0.09 secs, 82,196,272 bytes)

*Main> fib 30
832040
(0.96 secs, 910,823,744 bytes)

*Main> fib 31
1346269
(1.59 secs, 1,473,698,280 bytes)

*Main> fib 32
2178309
(2.51 secs, 2,384,447,160 bytes)

*Main> fib 33
3524578
(4.06 secs, 3,858,069,816 bytes)

*Main> fib 34
5702887
(6.51 secs, 6,242,441,368 bytes)
-}
fib :: (Eq n, Num n) => n -> n
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{- | unfoldr version

>>> fib' 10
55

*Main> fib' 10000
33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875
(0.01 secs, 9,179,488 bytes)

===

Here's what's going on:

*Main> (\(a, b) -> Just (a, (b, a + b))) (0,1)
Just (0,(1,1))
(0.00 secs, 82,368 bytes)

cons 0, try (1,1) => [0]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (1,1)
Just (1,(1,2))
(0.00 secs, 82,384 bytes)

cons 1, try (1,2) => [0,1]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (1,2)
Just (1,(2,3))
(0.00 secs, 82,384 bytes)

cons 1, try (2,3) => [0,1,1]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (2,3)
Just (2,(3,5))
(0.00 secs, 82,384 bytes)

cons 2, try (3,5) => [0,1,1,2]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (3,5)
Just (3,(5,8))
(0.00 secs, 82,384 bytes)

cons 3, try (5,8) => [0,1,1,2,3]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (5,8)
Just (5,(8,13))
(0.00 secs, 83,144 bytes)

cons 5, try (8,13) => [0,1,1,2,3,5]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (8,13)
Just (8,(13,21))

cons 8, try (13,21) => [0,1,1,2,3,5,8]

~~~

*Main> (\(a, b) -> Just (a, (b, a + b))) (13,21)
Just (13,(21,34))
(0.00 secs, 84,656 bytes)

cons 13, try (21,34) => [0,1,1,2,3,5,8,13]

~~~

*Main> take 8 $ unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)
[0,1,1,2,3,5,8,13]
-}
fib' :: (Num n) => Int -> n
fib' n =
  fibs !! n
 where
  fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

{- |

*Main> build (\cons nil -> 1 `cons` (2 `cons` (3 `cons` nil)))
[1,2,3]
-}

--------------------------------------------------------------------------------
-- Copied from source
--------------------------------------------------------------------------------
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
{-# INLINE unfoldr #-} -- See Note [INLINE unfoldr]
unfoldr f b0 =
  build
    ( \c n ->
        let go b = case f b of
              Just (a, new_b) -> a `c` go new_b
              Nothing -> n
         in go b0
    )

--------------------------------------------------------------------------------
-- Copied from source
--------------------------------------------------------------------------------
build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
-- The INLINE is important, even though build is tiny,
-- because it prevents [] getting inlined in the version that
-- appears in the interface file.  If [] *is* inlined, it
-- won't match with [] appearing in rules in an importing module.
--
-- The "1" says to inline in phase 1
build g = g (:) []

--------------------------------------------------------------------------------
{-
NOTE: unfoldr is not defined as in unfoldr2

Meaning, we don't actually build intermediary lists with a concrete cons and nil
implementation.

Instead, we use build to provide cons and nil in order to enable a "list fusion"
optimization technique.

See: https://markkarpov.com/tutorial/ghc-optimization-and-fusion#buildfoldr-fusion-system
-}
--------------------------------------------------------------------------------
unfoldr2 :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr2 f b0 =
  let go b = case f b of
        Just (a, new_b) -> a : go new_b
        Nothing -> []
   in go b0

{-

I got quite confused about "build" initially. This example made things clearer
for me.

We are provided cons and nil.
See the "How does it help to eliminate intermediate lists?" of the article to
understand how function substitution enables "fusion".

>>> build (\cons nil -> 1 `cons` (2 `cons` (3 `cons` nil)))
[1,2,3]
 -}

{- | Another method, found on: https://fr.wikipedia.org/wiki/Suite_de_Fibonacci

A bit of a mind bender! This one seems to be the best behaving implementation
(uses slightly less memory then fib' (using unfoldr) and fib''' (using uncurry (+)))
>>> fib'' 10
55
-}
fib'' :: (Num n) => Int -> n
fib'' n =
  fibs !! n
 where
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{- |

Variation of the prior method (basically the same but more verbose).

>>> uncurry (+) <$> zip [0,1,1,2,3,5,8,13] (tail [0,1,1,2,3,5,8,13])
[1,2,3,5,8,13,21]

>>> fib''' 10
55
-}
fib''' :: (Num n) => Int -> n
fib''' n =
  fibs !! n
 where
  fibs = 0 : 1 : (uncurry (+) <$> zip fibs (tail fibs))

{- |

Iterate seems actually quite nicer!

Found here: https://wiki.haskell.org/The_Fibonacci_sequence

Since we don't actually need to "stop" the building of the list, using "iterate"
makes more sens. We don't actually need "unfoldr".

*Main> f = (\(a,b) -> (b,a+b))
(0.00 secs, 69,616 bytes)
*Main> f (0,1)
(1,1)
(0.00 secs, 74,968 bytes)
*Main> f $ f (0,1)
(1,2)
(0.00 secs, 75,176 bytes)
*Main> f $ f $ f (0,1)
(2,3)
(0.00 secs, 75,384 bytes)
*Main> f $ f $ f $ f (0,1)
(3,5)
(0.00 secs, 75,584 bytes)
*Main> f $ f $ f $ f $ f (0,1)
(5,8)
(0.01 secs, 75,792 bytes)
*Main> f $ f $ f $ f $ f $ f (0,1)
(8,13)
(0.00 secs, 76,768 bytes)
*Main> f $ f $ f $ f $ f $ f $ f (0,1)
(13,21)

*Main> fst $ (iterate f (0,1)) !! 10
55
(0.00 secs, 74,496 bytes)
*Main> snd $ (iterate f (0,1)) !! 9
55

>>> fibIterate 10
55
-}
fibIterate :: (Num n) => Int -> n
fibIterate n =
  fibs !! n
 where
  next (a, b) = (b, a + b)
  fibs = fst <$> iterate next (0, 1)

------ compelling example for unfoldr ------

{- | Converting numbers to different bases
This MUST terminate when we reach 0

>>> toBase 2 5
[1,0,1]

>>> toBase 2 8
[1,0,0,0]


>>> toBase 16 255
[15,15]
-}
toBase :: Int -> Int -> [Int]
toBase base n = reverse $ unfoldr go n
 where
  go 0 = Nothing -- Termination condition!
  go x = Just (x `mod` base, x `div` base)

{- | Although we could also easily define toBase with iterate + takeWhile.

Using unfoldr is a little cleaner though.
>>> toBase' 2 5
[1,0,1]

>>> toBase' 2 8
[1,0,0,0]

>>> toBase' 16 255
[15,15]
-}
toBase' :: Int -> Int -> [Int]
toBase' base n = reverse $ (`mod` base) <$> takeWhile (/= 0) (iterate (`div` base) n)
