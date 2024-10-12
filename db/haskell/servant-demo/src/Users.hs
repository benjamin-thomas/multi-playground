{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Users (User, list) where

import Data.Aeson qualified as Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)

data User = User
    { id :: Int
    , firstName :: String
    , lastName :: String
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- encodeUser :: User -> ByteString
encode user = Aeson.encode user

list :: [User]
list =
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    , User{id = 3, firstName = "Ada", lastName = "Lovelace"}
    , User{id = 4, firstName = "Alan", lastName = "Turing"}
    ]