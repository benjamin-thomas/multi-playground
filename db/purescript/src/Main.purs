module Main
  ( main
  , program3
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut (class DecodeJson, Json, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Postgres (Client, Pool, Query(..), queryOne, query_)
import Yoga.Postgres as YP
import Yoga.Postgres.SqlValue (SqlValue, toSql)

type CustomerSimple =
  { id :: Int
  , name :: String
  }

type CustomerFull =
  { id :: Int
  , name :: String
  , alternative_name :: Maybe String
  }

cfg :: YP.ClientConfig
cfg =
  { host: "127.23.0.1"
  , database: "mpg_db"
  , port: 5432
  , user: "postgres"
  , password: "postgres"
  , ssl: false
  }

connInfo :: YP.ConnectionInfo
connInfo = YP.connectionInfoFromConfig cfg YP.defaultPoolConfig

read' ∷ ∀ (t ∷ Type). DecodeJson t ⇒ Foreign → Either Error t
read' = toJson >>> decodeJson >>> lmap toError
  where
  toJson :: Foreign -> Json
  toJson = unsafeCoerce

  toError = printJsonDecodeError >>> error

type MyError = String

safeQueryOne
  :: forall a
   . DecodeJson a
  => (Foreign -> Either Error a)
  -> Query a
  -> Array SqlValue
  -> Client
  -> Aff (Either MyError (Maybe a))
safeQueryOne decode q params client = do
  result <- try $ queryOne decode q params client
  pure $ case result of
    Left err -> Left $ "QueryOne Error: " <> show err
    Right res -> Right res

type Item = { n :: Int }

selectOne :: YP.Query Item
selectOne =
  Query "SELECT x as n FROM generate_series(1,3)x"

selectCustomers :: Query CustomerFull
selectCustomers =
  Query "SELECT * FROM customers ORDER BY id DESC LIMIT 3"

findCustomerByName :: Query CustomerSimple
findCustomerByName =
  Query "SELECT id, name FROM customers WHERE name = $1 LIMIT 1"

findCustomerByNameQuery ∷ Array SqlValue → Client → Aff (Maybe CustomerSimple)
findCustomerByNameQuery = queryOne read' findCustomerByName

queryItems :: Pool -> Aff (Array Item)
queryItems pool = do
  YP.withClient pool \client -> do
    -- nums <- queryValue_ read' selectOne client
    -- nums <- queryOne_ read' selectOne client
    query_ read' selectOne client

findJohn :: Pool -> Aff (Maybe CustomerSimple)
findJohn pool = do
  YP.withClient pool $ findCustomerByNameQuery [ toSql "John" ]

top3Customers :: Pool -> Aff (Array CustomerFull)
top3Customers pool = do
  YP.withClient pool \client -> do
    query_ read' selectCustomers client

program0 :: Pool -> Aff Unit
program0 pool = do
  items <- queryItems pool
  john <- findJohn pool
  mary1 <- YP.withClient pool $ queryOne read' findCustomerByName [ toSql "Mary" ]
  liftEffect $ log "[Program0] did get mary1" -- whole program0 crashes (but not main) if SQL error below
  mary2 <- YP.withClient pool $ queryOne read' (Query "SELECT id, name FROM customers WHERE name = $1 LIMIT 1" :: Query CustomerSimple) [ toSql "Mary" ]
  jane <- YP.withClient pool $ findCustomerByNameQuery [ toSql "Jane" ]
  top3 <- top3Customers pool
  liftEffect $ do
    log $ "[Program0] items: " <> show items
    log $ "[Program0] john: " <> show john
    log $ "[Program0] mary1: " <> show mary1
    log $ "[Program0] mary2: " <> show mary2
    log $ "[Program0] jane: " <> show jane
    log $ "[Program0] top3: " <> show top3
  pure unit

program1 :: Pool -> Aff Unit
program1 pool = do
  result <- try do
    items <- queryItems pool
    john <- findJohn pool
    mary1 <- YP.withClient pool $ queryOne read' findCustomerByName [ toSql "Mary" ]
    liftEffect $ log "[Program1] did get mary1" -- whole program1 crashes (but not main) if SQL error below
    mary2 <- YP.withClient pool $ queryOne read' (Query "SELECT id, name FROM customers WHERE name = $1 LIMIT 1" :: Query CustomerSimple) [ toSql "Mary" ]
    jane <- YP.withClient pool $ findCustomerByNameQuery [ toSql "Jane" ]
    top3 <- top3Customers pool
    pure $ (items /\ john /\ mary1 /\ mary2 /\ jane /\ top3)

  case result of
    Left err -> liftEffect $ log $ "[Program1] An error occurred: " <> show err
    Right (items /\ john /\ mary1 /\ mary2 /\ jane /\ top3) -> liftEffect $ do
      log $ "[Program1] items: " <> show items
      log $ "[Program1] john: " <> show john
      log $ "[Program1] mary1: " <> show mary1
      log $ "[Program1] mary2: " <> show mary2
      log $ "[Program1] jane: " <> show jane
      log $ "[Program1] top3: " <> show top3
  pure unit

program2 :: Pool -> Aff Unit
program2 pool = do
  result1 <- YP.withClient pool $ safeQueryOne read' findCustomerByName [ toSql "John" ]
  result2 <- YP.withClient pool $ safeQueryOne read' findCustomerByName [ toSql "Mary" ]
  case (result1 /\ result2) of
    Right john /\ Right mary -> liftEffect $ log $ "[Program2] john and mary are => " <> show john <> " and " <> show mary
    _ -> liftEffect $ log $ "[Program2] an error occurred"

-- Helper function to lift Aff actions into ExceptT
liftAff :: forall e a. Aff (Either e a) -> ExceptT e Aff a
liftAff action = ExceptT action

program3 :: Pool -> Aff (Maybe String)
program3 pool = do
  result <- runExceptT $ do
    john <- liftAff $ YP.withClient pool $ safeQueryOne read' findCustomerByName [ toSql "John" ]
    mary <- liftAff $ YP.withClient pool $ safeQueryOne read' (Query "SELECT id, namez FROM customers WHERE name = $1 LIMIT 1" :: Query CustomerSimple) [ toSql "Mary" ]
    liftEffect $ log $ "[Program3] john and mary are => " <> show john <> " and " <> show mary

  case result of
    Left err -> pure $ Just $ "[Program3] an error occurred: " <> err
    Right (_ :: Unit) -> pure Nothing

main :: Effect Unit
main = do
  pool <- liftEffect $ YP.mkPool connInfo
  launchAff_ $ program0 pool
  launchAff_ $ program1 pool
  launchAff_ $ program2 pool
  launchAff_ $ (\(_ :: Maybe String) -> pure unit) =<< program3 pool
  -- runAff_
  --   ( case _ of
  --       Nothing -> log "Program3 success"
  --       Just err -> log $ "Program3 error: " <> err
  --   )
  --   program3
  --   pool
  log "Finished"