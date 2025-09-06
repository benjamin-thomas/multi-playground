{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Applicative

red :: String
red = "\x1b[31m"

blue :: String
blue = "\x1b[34m"

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

{-

Using (->) r as the Reader pattern.

Each function has type: AppVars -> IO ()
We can compose them using Applicative/Monad operations on (->).

-}

newtype AppUser = MkAppUser String deriving (Show)

userToString :: AppUser -> String
userToString (MkAppUser user) = user

newtype AppPassword = MkAppPassword String deriving (Show, Eq)

data AppVars = MkAppVars
    { appUser :: AppUser
    , dbPassword :: AppPassword
    }
    deriving (Show)

-- Helper to extract username from environment
getUser :: AppVars -> AppUser
getUser = appUser

-- Helper to extract password from environment
getPassword :: AppVars -> AppPassword
getPassword = dbPassword

newAppVars :: AppVars
newAppVars =
    MkAppVars
        { appUser = MkAppUser "John"
        , dbPassword = MkAppPassword "123456"
        }

fakeMain :: IO ()
fakeMain = do
    let maxQty = 10
        appVars =
            MkAppVars
                { appUser = MkAppUser "John"
                , dbPassword = MkAppPassword "123456"
                }
     in addQuantityUI maxQty appVars

-- Type: Int -> AppVars -> IO ()
-- This is equivalent to: Int -> Reader AppVars (IO ())
addQuantityUI :: Int -> AppVars -> IO ()
addQuantityUI maxQty appVars = do
    putStrLn $ blue <> "Welcome back " <> userToString (getUser appVars) <> "!" <> reset
    putStr "Enter a new quantity: "
    qty <- read <$> getLine
    if qty >= maxQty
        then putStrLn $ red <> "Quantity must be less than " <> show maxQty <> reset
        else addQuantity qty appVars

-- We can also use the function monad to compose operations
addQuantityUI' :: Int -> AppVars -> IO ()
addQuantityUI' maxQty appVars = do
    putStrLn $ blue <> "Welcome back " <> userToString (getUser appVars) <> "!" <> reset
    putStr "Enter a new quantity: "
    qty <- read <$> getLine
    if qty >= maxQty
        then putStrLn $ red <> "Quantity must be less than " <> show maxQty <> reset
        else addQuantity qty appVars -- Partial application!

-- Using function composition and applicative style
welcomeMessage :: AppVars -> String
welcomeMessage appVars = blue <> "Welcome back " <> userToString (getUser appVars) <> "!" <> reset

-- We can combine multiple "readers" using applicative syntax
logMessage :: AppUser -> AppPassword -> String
logMessage (MkAppUser user) (MkAppPassword pass) =
    red
        <> "Bad password attempted with: "
        <> take 3 pass
        <> replicate (length pass - 3) '*'
        <> " (Username is "
        <> user
        <> ")"
        <> reset

-- Using applicative to combine the readers
logBadPassword :: AppVars -> IO ()
logBadPassword appVars = putStrLn $ logMessage (getUser appVars) (getPassword appVars)

-- Or using the applicative instance of (->) directly:
logBadPassword' :: AppVars -> IO ()
logBadPassword' = fmap putStrLn msg
  where
    msg :: AppVars -> String
    msg = logMessage <$> getUser <*> getPassword

-- Or just use the record functions directly
logBadPassword'' :: AppVars -> IO ()
logBadPassword'' = fmap putStrLn msg
  where
    msg :: AppVars -> String
    msg = logMessage <$> appUser <*> dbPassword

addQuantity :: Int -> AppVars -> IO ()
addQuantity qty appVars = do
    if getPassword appVars /= MkAppPassword "123456"
        then logBadPassword appVars
        else putStrLn $ green <> "Inserting " <> show qty <> " into the DB" <> reset

-- Using function monad for conditional logic
addQuantity' :: Int -> AppVars -> IO ()
addQuantity' qty = do
    -- This 'do' is in the (->) AppVars monad!
    MkAppPassword password <- getPassword -- password :: String, we "ask" for it
    if password /= "123456"
        then logBadPassword
        else const $ putStrLn $ green <> "Inserting " <> show qty <> " into the DB" <> reset

main :: IO ()
main = fakeMain