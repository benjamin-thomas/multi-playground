{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (
    StateT,
    execStateT,
    get,
    modify,
 )

{-

cabal install --lib transformers

Run with:
    echo ./Main.hs | entr -c runghc -wAll /_

 -}

data Cmd a where
    Pure :: a -> Cmd a
    Bind :: Cmd a -> (a -> Cmd b) -> Cmd b
    InviteUser :: String -> Cmd ()
    FetchUsers :: Cmd [String]
    Broadcast :: String -> Cmd ()

instance Functor Cmd where
    fmap :: (a -> b) -> Cmd a -> Cmd b
    fmap f g = Bind g (Pure . f)

instance Applicative Cmd where
    pure :: a -> Cmd a
    pure = Pure

    (<*>) :: Cmd (a -> b) -> Cmd a -> Cmd b
    (<*>) f g = Bind f (<$> g)

instance Monad Cmd where
    (>>=) :: Cmd a -> (a -> Cmd b) -> Cmd b
    (>>=) = Bind

program :: Cmd ()
program =
    let broadcastOnEven users =
            if even (length users)
                then
                    Broadcast "\x1b[1;36mThere are an even number of users\x1b[0m"
                else
                    Pure ()
     in do
            Broadcast "Booting up..."
            users1 <- FetchUsers
            broadcastOnEven users1

            InviteUser "@John"
            users2 <- FetchUsers
            broadcastOnEven users2

            InviteUser "@Jane"
            users3 <- FetchUsers
            broadcastOnEven users3
            Broadcast "Shutting down..."

data AppState = AppState
    { stCounter :: Int
    , stUsers :: [String]
    }
    deriving (Show)

type AppIO a = StateT AppState IO a

interpret :: Cmd a -> AppIO a
interpret command =
    case command of
        Pure a -> pure a
        Bind f g -> interpret f >>= interpret . g
        InviteUser userName -> do
            modify
                ( \st ->
                    st
                        { stUsers = userName : stUsers st
                        , stCounter = stCounter st + 1
                        }
                )
            liftIO $ putStrLn ("You invited " <> userName <> "!")
        FetchUsers -> do
            users <- stUsers <$> get
            liftIO $ putStrLn ("\x1b[1;33m" <> "==> Returning users: " <> "\x1b[0m" <> unwords users)
            pure users
        Broadcast val -> do
            counter <- stCounter <$> get
            liftIO $
                putStrLn $
                    mconcat
                        [ "Broadcasting: " <> quote val <> " "
                        , "(counter=" <> show counter <> ")"
                        ]
          where
            quote v = "'" <> v <> "'"

main :: IO ()
main =
    void $
        execStateT
            (interpret program)
            initialState
  where
    initialState =
        AppState
            { stUsers = ["@Bob"]
            , stCounter = 1
            }