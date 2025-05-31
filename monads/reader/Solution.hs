{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Reader

red :: String
red = "\x1b[31m"

blue :: String
blue = "\x1b[34m"

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

{-

Solution:

The Reader Monad is quite flexible in this regard. Using a tuple for clarity, but a record would probably be better?

---

runghc ./Solution.hs

-}

newtype AppVars
    = MkAppVars
        ( String
        , String
        )

fakeMain :: IO ()
fakeMain = do
    let appUser = "John"
        dbPassword = "123456"
        maxQty = 10
     in runReaderT
            (addQuantityUI maxQty)
            (MkAppVars (appUser, dbPassword))

addQuantityUI :: Int -> ReaderT AppVars IO ()
addQuantityUI maxQty = do
    MkAppVars (appUser, _dbPassword) <- ask
    liftIO $ putStrLn $ blue <> "Welcome back " <> appUser <> "!" <> reset
    liftIO $ putStr "Enter a new quantity: "
    qty <- liftIO $ read <$> getLine
    if qty >= maxQty
        then liftIO $ putStrLn $ red <> "Quantity must be less than " <> show maxQty <> reset
        else addQuantity qty

addQuantity :: Int -> ReaderT AppVars IO ()
addQuantity qty = do
    MkAppVars (_appUser, dbPassword) <- ask
    if dbPassword /= "123456"
        then logBadPassword
        else liftIO $ putStrLn $ green <> "Inserting " <> show qty <> " into the DB" <> reset

logBadPassword :: ReaderT AppVars IO ()
logBadPassword = do
    MkAppVars (appUser, dbPassword) <- ask
    liftIO $
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