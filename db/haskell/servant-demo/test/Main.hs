{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BC
import Data.CaseInsensitive qualified as CI
import MyLib (app, someOtherValue, someValue)
import Users qualified as Users
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Wai (
    ResponseMatcher (..),
    get,
    matchBody,
    shouldRespondWith,
    with,
 )
import Test.Hspec.Wai.Matcher (MatchHeader, bodyEquals, (<:>))

addition :: Spec
addition = do
    describe "(+)" $ do
        it "adds integers" $
            2 + 3 == (5 :: Integer)

        it "adds floats" $
            2.0 + 3.0 == (5.0 :: Float)

multiplication :: Spec
multiplication = do
    describe "(*)" $ do
        it "multiplies integers" $
            2 * 3 `shouldBe` (6 :: Integer)

        it "multiplies floats" $
            2.0 * 3.0 == (6.0 :: Float)

libFunctions :: Spec
libFunctions = do
    describe "lib functions" $ do
        it "someValue" $
            MyLib.someValue `shouldBe` 42

        it "someOtherValue" $
            MyLib.someOtherValue `shouldBe` "wat"

mkHeader :: String -> MatchHeader
mkHeader str = CI.mk (BC.pack "Content-Type") <:> BC.pack str

routingSpec :: Spec
routingSpec = with (return MyLib.app) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get (BC.pack "/") `shouldRespondWith` 200
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users =
                    [ User 1 "Isaac" "Newton"
                    , User 2 "Albert" "Einstein"
                    ]

            get (BC.pack "/users")
                `shouldRespondWith` ResponseMatcher
                    { matchStatus = 200
                    , matchHeaders =
                        [ mkHeader "application/json;charset=utf-8"
                        ]
                    , matchBody = bodyEquals $ MyLib.encodeUser users
                    }

main :: IO ()
main = do
    hspec $ do
        addition
        multiplication
        libFunctions
        routingSpec