{-# OPTIONS_GHC -Wno-unused-imports #-}

module ExploreReader where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.List (intercalate)

{- | A reader that takes a `String` and returns an `Int`.
The `String` is the "environment" of the `Reader`.

To get the `Int` value of the `Reader`, we "run" it:
>>> runReader r1 "2"
5
-}
r1 :: Reader String Int
r1 = return 5

{- |
>>> runReader accountStatus 1000
"Current balance: 1000 EUR"

>>> runReader accountStatus 0
"No money left :("

>>> runReader accountStatus (-1)
"Your account is in overdraft!"
-}
accountStatus :: Reader Int String
accountStatus = do
    env <- ask
    return $ case compare env 0 of
        GT -> "Current balance: " ++ show env ++ " EUR"
        EQ -> "No money left :("
        LT -> "Your account is in overdraft!"

mailboxStatus :: Reader Int String
mailboxStatus = do
    env <- ask
    return
        $ if env > 0
            then "You've got mail: " ++ show env
            else "Your mailbox is empty!"

{- | We can generate many outcomes from 1 environment

Which doesn't quite make sens here because our environment is not expressive enough.

>>> runReader appStatus 0
"No money left :( | Your mailbox is empty!"

>>> runReader appStatus 1000
"Current balance: 1000 EUR | You've got mail: 1000"
-}
appStatus :: Reader Int String
appStatus = do
    account <- accountStatus
    mailbox <- mailboxStatus
    return $ intercalate " | " [account, mailbox]

-- | So it's time to introduce an environment specific to our app
data AppState = AppState
    { bankBalance :: Int
    , unreadMailCount :: Int
    }
    deriving (Show)

initState :: AppState
initState = AppState{bankBalance = 1000, unreadMailCount = 3}

{- |
>>> runReader accountStatus2 initState
"Current balance: 1000 EUR"

>>> runReader accountStatus2 initState{bankBalance = 0}
"No money left :("

>>> runReader accountStatus2 AppState{bankBalance = -1, unreadMailCount = 0} -- or build the record fully
"Your account is in overdraft!"
-}
accountStatus2 :: Reader AppState String
accountStatus2 = do
    env <- ask
    return $ case compare (bankBalance env) 0 of
        GT -> "Current balance: " ++ show (bankBalance env) ++ " EUR"
        EQ -> "No money left :("
        LT -> "Your account is in overdraft!"

{- |
>>> runReader mailboxStatus2 initState
"You've got mail: 3"

>>> runReader mailboxStatus2 initState{unreadMailCount = 0}
"Your mailbox is empty!"
-}
mailboxStatus2 :: Reader AppState String
mailboxStatus2 = do
    env <- ask
    return
        $ if unreadMailCount env > 0
            then "You've got mail: " ++ show (unreadMailCount env)
            else "Your mailbox is empty!"

{- | Let's generate again many outcomes from our environment.

>>> runReader appStatus2 initState
"Current balance: 1000 EUR | You've got mail: 3"

>>> runReader appStatus2 initState{bankBalance = 0}
"No money left :( | You've got mail: 3"


>>> runReader appStatus2 AppState{bankBalance = -1, unreadMailCount = 0}
"Your account is in overdraft! | Your mailbox is empty!"

>>> runReader appStatus2 AppState{bankBalance = -1, unreadMailCount = 999}
"Your account is in overdraft! | You've got mail: 999"
-}
appStatus2 :: Reader AppState String
appStatus2 = do
    account <- accountStatus2
    mailbox <- mailboxStatus2
    return $ intercalate " | " [account, mailbox]
