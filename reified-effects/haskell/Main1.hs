{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Random (randomRIO)

-- cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple reified-effects-scattered

--------------------------------------------------------------------------------
-- DOMAIN: A simple user registration system
--------------------------------------------------------------------------------

-- Our application model (pure data)
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
-- EFFECTS AS DATA (reified!)
-- These are just descriptions of what we want to happen
--------------------------------------------------------------------------------

newtype Recipient = Recipient String deriving (Show, Eq)
newtype EmailMessage = EmailMessage String deriving (Show, Eq)
newtype LogMessage = LogMessage String deriving (Show, Eq)
newtype Notification = Notification String deriving (Show, Eq)
newtype Url = Url String deriving (Show, Eq)
newtype UserId = UserId Int deriving (Show, Eq)

data Effect
  = Log LogMessage
  | SaveUser User
  | SendEmail Recipient EmailMessage
  | ShowNotification Notification
  | Navigate Url
  | None
  | Batch [Effect]
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- ACTIONS (like Elm messages)
--------------------------------------------------------------------------------

newtype Username = Username String deriving (Show, Eq)
newtype Email = Email String deriving (Show, Eq)

data Action
  = RegisterClicked Username Email
  | UserSaved UserId -- Async callback when save completes
  | LoginClicked Username
  | LogoutClicked
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- UPDATE - PURE! Returns new model + effects as data
--------------------------------------------------------------------------------

update :: Model -> Action -> (Model, Effect)
update model action = case action of
  RegisterClicked (Username username') (Email email') ->
    let user = User username' email'
     in ( model{currentUser = Just user} -- Optimistically set user
        , Batch
            [ Log $ LogMessage ("Registering user: " ++ username')
            , ShowNotification $ Notification "Creating account..."
            , SaveUser user -- When done, interpreter will dispatch UserSaved
            ]
        )
  UserSaved userId ->
    -- This runs AFTER save completes (scattered from RegisterClicked!)
    case currentUser model of
      Nothing -> (model, None)
      Just user ->
        ( model{users = Map.insert (username user) user (users model)}
        , Batch
            [ Log $ LogMessage ("User saved with ID: " ++ show userId)
            , SendEmail
                (Recipient (email user))
                (EmailMessage $ "Welcome! Your user ID is: " ++ show userId)
            , ShowNotification $ Notification "Account created!"
            , Navigate $ Url "/dashboard"
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
-- INTERPRETER - The ONLY place with IO!
-- Converts our effect data into actual IO operations
--
-- The interpreter closes the loop for async operations:
--   Action → update → Effect → interpret → Action → update → ...
--
-- When async effects complete, they dispatch new actions back to update
--------------------------------------------------------------------------------

storeUserInDb :: User -> IO UserId
storeUserInDb user = do
  putStrLn $ "[DB] Storing user in database: " ++ username user
  threadDelay 500000
  userId <- randomRIO (1000, 9999)
  pure $ UserId userId

interpret :: (Action -> IO ()) -> Effect -> IO ()
interpret dispatch = \case
  Log (LogMessage msg) ->
    putStrLn $ "[LOG] " ++ msg
  SaveUser user -> do
    putStrLn $ "[DB] Starting async save for user: " ++ username user
    -- Run async
    handle <- async $ storeUserInDb user
    userId <- wait handle -- Wait for completion
    putStrLn $ "[DB] Save completed with ID: " ++ show userId
    -- Dispatch action to continue the flow (back through update!)
    dispatch (UserSaved userId)
  SendEmail (Recipient recipient) (EmailMessage msg) -> do
    putStrLn $ "[EMAIL] To: " ++ recipient ++ " | Message: " ++ msg
  ShowNotification (Notification msg) ->
    putStrLn $ "[UI] Notification: " ++ msg
  Navigate (Url url) ->
    putStrLn $ "[NAV] Navigating to: " ++ url
  None ->
    pure ()
  Batch effects ->
    mapM_ (interpret dispatch) effects

--------------------------------------------------------------------------------
-- TESTING - Pure, no IO needed!
--------------------------------------------------------------------------------

testRegisterUser :: ((Model, Effect), (Model, Effect))
testRegisterUser =
  let got = update initialModel (RegisterClicked (Username "alice") (Email "alice@example.com"))
      expected =
        ( Model
            { users = Map.empty
            , currentUser =
                Just
                  ( User
                      { username = "alice"
                      , email = "alice@example.com"
                      }
                  )
            }
        , Batch
            [ Log $ LogMessage "Registering user: alice"
            , ShowNotification $ Notification "Creating account..."
            , SaveUser (User "alice" "alice@example.com")
            ]
        )
   in (got, expected)

testLoginSuccess :: ((Model, Effect), (Model, Effect))
testLoginSuccess =
  let modelWithUser =
        initialModel
          { users = Map.singleton "alice" (User "alice" "alice@example.com")
          }
      got = update modelWithUser (LoginClicked (Username "alice"))
      expected =
        ( modelWithUser{currentUser = Just (User "alice" "alice@example.com")}
        , Batch
            [ Log $ LogMessage "User logged in: alice"
            , ShowNotification $ Notification "Welcome back, alice!"
            , Navigate $ Url "/dashboard"
            ]
        )
   in (got, expected)

testLoginFailure :: ((Model, Effect), (Model, Effect))
testLoginFailure =
  let got = update initialModel (LoginClicked (Username "bob"))
      expected = (initialModel, ShowNotification $ Notification "User not found")
   in (got, expected)

testLogout :: ((Model, Effect), (Model, Effect))
testLogout =
  let modelLoggedIn =
        initialModel
          { currentUser = Just (User "alice" "alice@example.com")
          }
      got = update modelLoggedIn LogoutClicked
      expected =
        ( initialModel -- currentUser set to Nothing
        , Batch
            [ Log $ LogMessage "User logged out"
            , ShowNotification $ Notification "Goodbye!"
            , Navigate $ Url "/login"
            ]
        )
   in (got, expected)

runTests :: IO ()
runTests = do
  putStrLn "\n=== Running Tests ==="
  let (gotReg, expReg) = testRegisterUser
  putStrLn $ "Register user: " ++ show (gotReg == expReg)
  let (gotLogin, expLogin) = testLoginSuccess
  putStrLn $ "Login success: " ++ show (gotLogin == expLogin)
  let (gotLoginFail, expLoginFail) = testLoginFailure
  putStrLn $ "Login failure: " ++ show (gotLoginFail == expLoginFail)
  let (gotLogout, expLogout) = testLogout
  putStrLn $ "Logout: " ++ show (gotLogout == expLogout)
  putStrLn "===================\n"

--------------------------------------------------------------------------------
-- RUNTIME - Simulating event loop
--------------------------------------------------------------------------------

-- Simulate the app running
runtime :: Model -> [Action] -> IO Model
runtime model [] = pure model
runtime model (action : actions) = do
  putStrLn $ "\n--- Action: " ++ show action ++ " ---"
  let (newModel, effects) = update model action
  -- Dispatch function that continues the runtime loop with new actions
  let dispatch newAction = do
        putStrLn $ "\n>>> Dispatched: " ++ show newAction
        _ <- runtime newModel [newAction]
        pure ()
  interpret dispatch effects
  putStrLn $ "New model: " ++ show newModel
  runtime newModel actions

--------------------------------------------------------------------------------
-- MAIN - Demo the app
--------------------------------------------------------------------------------

main :: IO ()
main = do
  runTests

  putStrLn "\n=== Running Application ==="

  let scenario =
        [ RegisterClicked (Username "alice") (Email "alice@example.com")
        , -- No need for RegistrationSaved - callback handles it!
          LogoutClicked
        , LoginClicked (Username "alice")
        , LogoutClicked
        , LoginClicked (Username "bob") -- This should fail
        ]

  _ <- runtime initialModel scenario

  putStrLn "\n=== Done ==="
