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

Using the simple solution to pass a "config" record around.

This is equivalent in terms of verbosity as adding the reader monad in the signature.

So, either we change the return type with ReaderT, or we pass a config record around

---

runghc ./Problem.hs

-}

data AppVars = MkAppVars
    { appUser :: String
    , dbPassword :: String
    }

fakeMain :: IO ()
fakeMain = do
    let maxQty = 10
     in addQuantityUI
            ( MkAppVars
                { appUser = "John"
                , dbPassword = "123456"
                }
            )
            maxQty

addQuantityUI :: AppVars -> Int -> IO ()
addQuantityUI appVars maxQty = do
    putStrLn $ blue <> "Welcome back " <> appUser appVars <> "!" <> reset
    putStr "Enter a new quantity: "
    qty <- read <$> getLine
    if qty >= maxQty
        then putStrLn $ red <> "Quantity must be less than " <> show maxQty <> reset
        else addQuantity appVars qty

addQuantity :: AppVars -> Int -> IO ()
addQuantity appVars qty = do
    if dbPassword appVars /= "123456"
        then logBadPassword appVars
        else putStrLn $ green <> "Inserting " <> show qty <> " into the DB" <> reset

logBadPassword :: AppVars -> IO ()
logBadPassword appVars = do
    putStrLn $
        mconcat
            [ red
            , "Bad password attempted with: "
            , take 3 (dbPassword appVars)
            , replicate (length (dbPassword appVars) - 3) '*'
            , " (" <> "Username is " <> appUser appVars <> ")"
            , reset
            ]

main :: IO ()
main = fakeMain