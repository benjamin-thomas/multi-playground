{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Random (randomRIO)

-- cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple reified-effects-free

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
-- CHAIN (aka Free Monad)
-- Note: This is technically a "Free Monad" in category theory terms
-- We rename it to "Chain" for clarity - it represents sequential computation
--------------------------------------------------------------------------------

-- Chain represents a sequence of operations
data Chain f a
  = Done a -- Computation finished with a result
  | AndThen (f (Chain f a)) -- Do this effect, then continue with the rest

-- Show instance for Chain
instance (Show a, Show (f (Chain f a))) => Show (Chain f a) where
  show (Done a) = "Done " ++ show a
  show (AndThen fa) = "AndThen (" ++ show fa ++ ")"

-- Eq instance for Chain
instance (Eq a, Eq (f (Chain f a))) => Eq (Chain f a) where
  Done a == Done b = a == b
  AndThen fa == AndThen fb = fa == fb
  _ == _ = False

instance (Functor f) => Functor (Chain f) where
  fmap f (Done a) = Done (f a)
  fmap f (AndThen fa) = AndThen (fmap (fmap f) fa)

instance (Functor f) => Applicative (Chain f) where
  pure = Done
  Done f <*> Done a = Done (f a)
  Done f <*> AndThen fa = AndThen (fmap (f <$>) fa)
  AndThen ff <*> fa = AndThen (fmap (<*> fa) ff)

instance (Functor f) => Monad (Chain f) where
  return = Done
  Done a >>= f = f a
  AndThen fa >>= f = AndThen (fmap (>>= f) fa)

--------------------------------------------------------------------------------
-- EFFECTS AS DATA (with 'next' continuation parameter)
--------------------------------------------------------------------------------

newtype Recipient = Recipient String deriving (Show, Eq)
newtype EmailMessage = EmailMessage String deriving (Show, Eq)
newtype LogMessage = LogMessage String deriving (Show, Eq)
newtype Notification = Notification String deriving (Show, Eq)
newtype Url = Url String deriving (Show, Eq)
newtype UserId = UserId Int deriving (Show, Eq)

-- Effect functor - notice the 'next' parameter that threads through
data EffectF next
  = Log LogMessage next
  | SaveUser User (UserId -> next) -- Callback receives result
  | SendEmail Recipient EmailMessage next
  | ShowNotification Notification next
  | Navigate Url next
  deriving (Functor) -- Can derive Functor automatically!

-- Manual Show instance (show <callback> for functions)
instance (Show next) => Show (EffectF next) where
  show (Log msg next) = "Log " ++ show msg ++ " (" ++ show next ++ ")"
  show (SaveUser user _) = "SaveUser " ++ show user ++ " <callback>"
  show (SendEmail r m next) = "SendEmail " ++ show r ++ " " ++ show m ++ " (" ++ show next ++ ")"
  show (ShowNotification n next) = "ShowNotification " ++ show n ++ " (" ++ show next ++ ")"
  show (Navigate url next) = "Navigate " ++ show url ++ " (" ++ show next ++ ")"

-- Manual Eq instance (can't compare functions, so SaveUser never equals)
instance (Eq next) => Eq (EffectF next) where
  Log msg1 next1 == Log msg2 next2 = msg1 == msg2 && next1 == next2
  SendEmail r1 m1 next1 == SendEmail r2 m2 next2 = r1 == r2 && m1 == m2 && next1 == next2
  ShowNotification n1 next1 == ShowNotification n2 next2 = n1 == n2 && next1 == next2
  Navigate url1 next1 == Navigate url2 next2 = url1 == url2 && next1 == next2
  _ == _ = False -- SaveUser can't be compared due to function

-- Type alias for convenience
type Effect a = Chain EffectF a

-- Smart constructors that lift effects into Chain
logMsg :: LogMessage -> Effect ()
logMsg msg = AndThen (Log msg (Done ()))

saveUser :: User -> Effect UserId
saveUser user = AndThen (SaveUser user Done)

sendEmail :: Recipient -> EmailMessage -> Effect ()
sendEmail r m = AndThen (SendEmail r m (Done ()))

showNotification :: Notification -> Effect ()
showNotification n = AndThen (ShowNotification n (Done ()))

navigate :: Url -> Effect ()
navigate url = AndThen (Navigate url (Done ()))

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
-- UPDATE - Returns a Free monad computation!
--------------------------------------------------------------------------------

update :: Model -> Action -> (Model, Effect ())
update model action = case action of
  RegisterClicked (Username username') (Email email') ->
    let user = User username' email'
        -- This is monadic do-notation! Sequential effects!
        effects = do
          logMsg $ LogMessage ("Registering user: " ++ username')
          showNotification $ Notification "Creating account..."
          userId <- saveUser user -- Wait for userId!
          -- Now we have userId in scope
          sendEmail
            (Recipient email')
            (EmailMessage $ "Welcome! Your user ID is: " ++ show userId)
          showNotification $ Notification "Account created!"
          navigate $ Url "/dashboard"
     in (model{currentUser = Just user}, effects)
  LoginClicked (Username username') ->
    case Map.lookup username' (users model) of
      Nothing ->
        ( model
        , showNotification $ Notification "User not found"
        )
      Just user ->
        let effects = do
              logMsg $ LogMessage ("User logged in: " ++ username')
              showNotification $ Notification ("Welcome back, " ++ username' ++ "!")
              navigate $ Url "/dashboard"
         in (model{currentUser = Just user}, effects)
  LogoutClicked ->
    let effects = do
          logMsg $ LogMessage "User logged out"
          showNotification $ Notification "Goodbye!"
          navigate $ Url "/login"
     in (model{currentUser = Nothing}, effects)

--------------------------------------------------------------------------------
-- INTERPRETER - Pattern match on the Free structure
--------------------------------------------------------------------------------

storeUserInDb :: User -> IO UserId
storeUserInDb user = do
  putStrLn $ "[DB] Storing user in database: " ++ username user
  threadDelay 500000
  userId <- randomRIO (1000, 9999)
  pure $ UserId userId

-- Interpret the Chain
interpret :: Effect a -> IO a
interpret = \case
  Done a -> pure a -- Base case: computation done
  AndThen continuation -> case continuation of
    Log (LogMessage msg) next -> do
      putStrLn $ "[LOG] " ++ msg
      interpret next
    SaveUser user next -> do
      putStrLn $ "[DB] Starting async save for user: " ++ username user
      handle <- async $ storeUserInDb user
      userId <- wait handle
      putStrLn $ "[DB] Save completed with ID: " ++ show userId
      interpret (next userId) -- Continue with userId
    SendEmail (Recipient recipient) (EmailMessage msg) next -> do
      putStrLn $ "[EMAIL] To: " ++ recipient ++ " | Message: " ++ msg
      interpret next
    ShowNotification (Notification msg) next -> do
      putStrLn $ "[UI] Notification: " ++ msg
      interpret next
    Navigate (Url url) next -> do
      putStrLn $ "[NAV] Navigating to: " ++ url
      interpret next

--------------------------------------------------------------------------------
-- TESTING - We can inspect the Chain structure directly!
--------------------------------------------------------------------------------

-- Test by comparing the actual data structures
testRegisterUser :: ((Model, Effect ()), (Model, Effect ()))
testRegisterUser =
  let got = update initialModel (RegisterClicked (Username "alice") (Email "alice@example.com"))
      -- Build the expected Chain structure
      user = User "alice" "alice@example.com"
      expected =
        ( Model
            { users = Map.empty
            , currentUser = Just user
            }
        , AndThen
            ( Log
                (LogMessage "Registering user: alice")
                ( AndThen
                    ( ShowNotification
                        (Notification "Creating account...")
                        ( AndThen
                            ( SaveUser
                                user
                                ( \_userId ->
                                    -- This is the callback - we can't test it directly due to function equality
                                    -- But we can test the structure up to this point
                                    Done ()
                                )
                            )
                        )
                    )
                )
            )
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
