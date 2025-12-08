{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Random (randomRIO)

-- cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple reified-effects-callbacks

--------------------------------------------------------------------------------
-- DOMAIN: A simple user registration system
--------------------------------------------------------------------------------

data Model = Model
  { users :: Map String User
  , currentUser :: Maybe User
  }
  deriving (Show, Eq)

data User = User
  { username :: String
  , email :: String
  }
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { users = Map.empty
    , currentUser = Nothing
    }

--------------------------------------------------------------------------------
-- EFFECTS AS DATA WITH CALLBACKS
-- Callbacks keep the flow together in one action!
--------------------------------------------------------------------------------

newtype Recipient = Recipient String deriving (Show, Eq)
newtype EmailMessage = EmailMessage String deriving (Show, Eq)
newtype LogMessage = LogMessage String deriving (Show, Eq)
newtype Notification = Notification String deriving (Show, Eq)
newtype Url = Url String deriving (Show, Eq)
newtype UserId = UserId Int deriving (Show, Eq)

data Effect
  = Log LogMessage
  | SaveUser User (UserId -> Effect) -- Callback receives result and returns next Effect!
  | SendEmail Recipient EmailMessage
  | ShowNotification Notification
  | Navigate Url
  | None
  | Batch [Effect]

-- Manual Show instance (show <callback> for functions)
instance Show Effect where
  show (Log msg) = "Log " ++ show msg
  show (SaveUser user _) = "SaveUser " ++ show user ++ " <callback>"
  show (SendEmail r m) = "SendEmail " ++ show r ++ " " ++ show m
  show (ShowNotification n) = "ShowNotification " ++ show n
  show (Navigate url) = "Navigate " ++ show url
  show None = "None"
  show (Batch effects) = "Batch " ++ show effects

-- Can't derive Eq due to function, but we can make a manual one
instance Eq Effect where
  Log msg1 == Log msg2 = msg1 == msg2
  SendEmail r1 m1 == SendEmail r2 m2 = r1 == r2 && m1 == m2
  ShowNotification n1 == ShowNotification n2 = n1 == n2
  Navigate url1 == Navigate url2 = url1 == url2
  None == None = True
  Batch e1 == Batch e2 = e1 == e2
  _ == _ = False -- SaveUser can't be compared due to callback

--------------------------------------------------------------------------------
-- ACTIONS
--------------------------------------------------------------------------------

newtype Username = Username String deriving (Show, Eq)
newtype Email = Email String deriving (Show, Eq)

data Action
  = RegisterClicked Username Email
  | LoginClicked Username
  | LogoutClicked
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- UPDATE - Returns effects with callbacks!
-- Flow stays together in one action thanks to callbacks
--------------------------------------------------------------------------------

update :: Model -> Action -> (Model, Effect)
update model action = case action of
  RegisterClicked (Username username') (Email email') ->
    let user = User username' email'
     in ( model{currentUser = Just user}
        , Batch
            [ Log $ LogMessage ("Registering user: " ++ username')
            , ShowNotification $ Notification "Creating account..."
            , SaveUser user $ \userId ->
                -- Callback keeps the flow together!
                Batch
                  [ Log $ LogMessage ("User saved with ID: " ++ show userId)
                  , SendEmail
                      (Recipient email')
                      (EmailMessage $ "Welcome! Your user ID is: " ++ show userId)
                  , ShowNotification $ Notification "Account created!"
                  , Navigate $ Url "/dashboard"
                  ]
            ]
        )
  LoginClicked (Username username') ->
    case Map.lookup username' (users model) of
      Nothing ->
        ( model
        , ShowNotification $ Notification "User not found"
        )
      Just user ->
        ( model{currentUser = Just user}
        , Batch
            [ Log $ LogMessage ("User logged in: " ++ username')
            , ShowNotification $ Notification ("Welcome back, " ++ username' ++ "!")
            , Navigate $ Url "/dashboard"
            ]
        )
  LogoutClicked ->
    ( model{currentUser = Nothing}
    , Batch
        [ Log $ LogMessage "User logged out"
        , ShowNotification $ Notification "Goodbye!"
        , Navigate $ Url "/login"
        ]
    )

--------------------------------------------------------------------------------
-- INTERPRETER - Pattern match and execute effects
--------------------------------------------------------------------------------

storeUserInDb :: User -> IO UserId
storeUserInDb user = do
  putStrLn $ "[DB] Storing user in database: " ++ username user
  threadDelay 500000
  userId <- randomRIO (1000, 9999)
  pure $ UserId userId

interpret :: Effect -> IO ()
interpret = \case
  Log (LogMessage msg) ->
    putStrLn $ "[LOG] " ++ msg
  SaveUser user callback -> do
    putStrLn $ "[DB] Starting async save for user: " ++ username user
    handle <- async $ storeUserInDb user
    userId <- wait handle
    putStrLn $ "[DB] Save completed with ID: " ++ show userId
    -- Invoke callback to get next effect, then interpret it!
    interpret (callback userId)
  SendEmail (Recipient recipient) (EmailMessage msg) ->
    putStrLn $ "[EMAIL] To: " ++ recipient ++ " | Message: " ++ msg
  ShowNotification (Notification msg) ->
    putStrLn $ "[UI] Notification: " ++ msg
  Navigate (Url url) ->
    putStrLn $ "[NAV] Navigating to: " ++ url
  None ->
    pure ()
  Batch effects ->
    mapM_ interpret effects

--------------------------------------------------------------------------------
-- TESTING - We can test the structure, but not what's in the callback
--------------------------------------------------------------------------------

testRegisterUser :: ((Model, Effect), (Model, Effect))
testRegisterUser =
  let got = update initialModel (RegisterClicked (Username "alice") (Email "alice@example.com"))
      user = User "alice" "alice@example.com"
      expected =
        ( Model
            { users = Map.empty
            , currentUser = Just user
            }
        , Batch
            [ Log $ LogMessage "Registering user: alice"
            , ShowNotification $ Notification "Creating account..."
            , SaveUser user $ \_userId ->
                -- Can't test what's in here due to function equality
                -- But we can test the structure up to this point
                None
            ]
        )
   in (got, expected)

--------------------------------------------------------------------------------
-- RUNTIME
--------------------------------------------------------------------------------

runtime :: Model -> [Action] -> IO Model
runtime model [] = pure model
runtime model (action : actions) = do
  putStrLn $ "\n--- Action: " ++ show action ++ " ---"
  let (newModel, effects) = update model action
  interpret effects
  putStrLn $ "New model: " ++ show newModel
  runtime newModel actions

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Testing ==="
  let (got, expected) = testRegisterUser
  putStrLn $ "Test passed: " ++ show (got == expected)

  putStrLn "\n\n=== Running Application ==="

  let scenario =
        [ RegisterClicked (Username "alice") (Email "alice@example.com")
        , LogoutClicked
        , LoginClicked (Username "alice")
        ]

  _ <- runtime initialModel scenario

  putStrLn "\n=== Done ==="
