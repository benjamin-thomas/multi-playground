{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc, someValue, someOtherValue, startApp, app, User (..), encodeUser) where

import Data.Aeson (encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant (
    Get,
    JSON,
    PlainText,
    Proxy (..),
    Server,
    serve,
    (:<|>) ((:<|>)),
    (:>),
 )
import Text.Printf (printf)

someValue :: Int
someValue = 42

someOtherValue :: String
someOtherValue = "wat"

someFunc :: IO ()
someFunc = putStrLn "someFunc3"

-- startApp :: Int -> IO ()
-- startApp port = run port app

data User = User
    { id :: Int
    , userFirstName :: String
    , userLastName :: String
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- encodeUser :: User -> ByteString
encodeUser user = encode user

data ProgrammingLanguage = ProgrammingLanguage
    { langId :: Int
    , langName :: String
    , langReleasedOn :: Int
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''ProgrammingLanguage)

startApp :: Int -> IO ()
startApp port = do
    withStdoutLogger $ \logger -> do
        let settings = setPort port $ setLogger logger defaultSettings
        printf "Listening on port %d\n" port
        runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type RootEndpoint = Get '[PlainText] String

type API =
    RootEndpoint
        :<|> "users" :> Get '[JSON] [User]
        :<|> "langs" :> Get '[JSON] [ProgrammingLanguage]

server :: Server API
server =
    return root
        :<|> return users
        :<|> return programmingLanguages

root :: String
root = "Hello!"

users :: [User]
users =
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    ]

programmingLanguages :: [ProgrammingLanguage]
programmingLanguages =
    [ ProgrammingLanguage 1 "Haskell" 2010
    , ProgrammingLanguage 2 "C++" 1980
    ]