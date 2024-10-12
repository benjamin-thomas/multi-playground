{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (
    someFunc,
    someValue,
    someOtherValue,
    startApp,
    app,
) where

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
import Users (User)
import Users qualified as Users

someValue :: Int
someValue = 42

someOtherValue :: String
someOtherValue = "wat"

someFunc :: IO ()
someFunc = putStrLn "someFunc3"

-- startApp :: Int -> IO ()
-- startApp port = run port app

data ProgrammingLanguage = ProgrammingLanguage
    { id :: Int
    , name :: String
    , releasedOn :: Int
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
type UsersEndpoint = "users" :> Get '[JSON] [User]
type LangsEndpoint = "langs" :> Get '[JSON] [ProgrammingLanguage]

type API =
    RootEndpoint
        :<|> UsersEndpoint
        :<|> LangsEndpoint

server :: Server API
server =
    return root
        :<|> return Users.list
        :<|> return programmingLanguages

root :: String
root = "Hello!"

programmingLanguages :: [ProgrammingLanguage]
programmingLanguages =
    [ ProgrammingLanguage 1 "Haskell" 2010
    , ProgrammingLanguage 2 "C++" 1980
    ]