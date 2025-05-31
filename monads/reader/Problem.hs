{-# OPTIONS_GHC -Wall #-}

module Main (main) where

red :: String
red = "\x1b[31m"

blue :: String
blue = "\x1b[34m"

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

{-

Problem:

We have to pass arguments over and over.
Worse, some functions, such as `addQuantityUI` only receive arguments to pass them right through, making even more boilerplate.

---

runghc ./Problem.hs

-}

fakeMain :: IO ()
fakeMain = do
    let appUser = "John"
        dbPassword = "123456"
        maxQty = 10
     in addQuantityUI appUser dbPassword maxQty

addQuantityUI :: String -> String -> Int -> IO ()
addQuantityUI appUser dbPassword maxQty = do
    putStrLn $ blue <> "Welcome back " <> appUser <> "!" <> reset
    putStr "Enter a new quantity: "
    qty <- read <$> getLine
    if qty >= maxQty
        then putStrLn $ red <> "Quantity must be less than " <> show maxQty <> reset
        else addQuantity appUser dbPassword qty

addQuantity :: String -> String -> Int -> IO ()
addQuantity appUser dbPassword qty = do
    if dbPassword /= "123456"
        then logBadPassword appUser dbPassword
        else putStrLn $ green <> "Inserting " <> show qty <> " into the DB" <> reset

logBadPassword :: String -> String -> IO ()
logBadPassword appUser dbPassword = do
    putStrLn $
        mconcat
            [ red
            , "Bad password attempted with: "
            , take 3 dbPassword
            , replicate (length dbPassword - 3) '*'
            , " (" <> "Username is " <> appUser <> ")"
            , reset
            ]

main :: IO ()
main = fakeMain