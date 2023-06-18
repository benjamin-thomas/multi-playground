{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (
  FromJSON (parseJSON),
  Value (Object),
  eitherDecode,
  withObject,
  (.:),
 )
import Data.Aeson.Lens (key)
import qualified Data.ByteString as B2
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Maybe as List
import Data.Text (Text)
import Data.Time ()
import GHC.Generics (Generic)
import Network.Wreq (defaults, getWith, header, param, responseBody)
import System.Environment (getArgs, getEnv, lookupEnv)

-- cabal run httpc 5Z00506074154

data Root = Root
  { lang :: Text
  , returnCode :: Int
  , shipment :: Shipment
  }
  deriving (Show)

newtype Shipment = Shipment
  {event :: [Event]}
  deriving (Show)

data Event = Event
  { code :: Text
  , label :: Text
  , date :: Text
  }
  deriving (Show)

{- FOURMOLU_DISABLE -}
instance FromJSON Root where
  parseJSON (Object v) =
    Root
      <$> v .: "lang"
      <*> v .: "returnCode"
      <*> v .: "shipment"

instance FromJSON Shipment where
  parseJSON = withObject "shipment" $ \v ->
    Shipment
      <$> v .: "event"

instance FromJSON Event where
  parseJSON = withObject "event" $ \v ->
    Event
      <$> v .: "code"
      <*> v .: "label"
      <*> v .: "date"
{- FOURMOLU_ENABLE -}

fetch :: B2.ByteString -> String -> IO ()
fetch apiKey tracking =
  let
    opts =
      defaults
        & param "lang" .~ ["fr_FR"]
        & header "Accept" .~ ["application/json"]
        & header "X-Okapi-Key" .~ [apiKey]
   in
    do
      resp <- getWith opts ("https://api.laposte.fr/suivi/v2/idships/" ++ tracking)
      let json = (resp ^. responseBody :: B.ByteString)
      case eitherDecode json of
        Left err -> putStrLn $ "Error: " ++ err
        Right root -> print (root :: Root)
      putStrLn "Done!"

main :: IO ()
main = do
  apiKey <- getEnv "API_KEY" -- FIXME: raises
  [tracking] <- getArgs -- FIXME: raises
  fetch (C8.pack apiKey) tracking
  putStrLn "Finished!"
