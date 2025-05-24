{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (Exception)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TLIO
import Network.HTTP.Types (badRequest400)
import Network.Wai.Middleware.RequestLogger
import Text.Read (readMaybe)

-- import Web.Scotty

import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text, toStrict)
import Web.Scotty (scotty)
import Web.Scotty.Trans

data AppExceptions
    = BadInput
    deriving (Show)

instance Exception AppExceptions

pathParamInt :: (MonadIO m) => Text -> ActionT m Int
pathParamInt str = do
    mn <- pathParam (toStrict str)
    case readMaybe mn of
        Nothing -> do
            throw BadInput
        Just n ->
            pure n

handleNum :: (MonadIO m, Show n, Num n) => n -> ActionT m ()
handleNum num = text . T.pack . show $ num - 1

main :: IO ()
main = scotty 1234 $ do
    middleware logStdout

    get "/" $ do
        text "Hello, Scotty!"

    get "/greet" $ do
        redirect "/hello/stranger"

    get "/hello/:name" $ do
        name <- pathParam "name"
        liftIO $ TLIO.putStrLn $ "name=" <> name
        text $ "Hello, " <> name <> "!"

    get "/inc/:num" $ do
        num <- pathParam "num"
        let num' = readMaybe num :: Maybe Int
        case num' of
            Nothing -> do
                status badRequest400
                text "Bad request"
            Just n ->
                text . T.pack . show $ n + 1

    get "/dec/:num" $ do
        num <- pathParamInt "num"
        text . T.pack . show $ num - 1

    get "/mul/:num" $ do
        (num :: Int) <- pathParam "num"
        handleNum num
