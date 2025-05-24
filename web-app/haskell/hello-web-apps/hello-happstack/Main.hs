{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (msum, mzero)
import Happstack.Server
    ( Conf (port)
    , FromReqURI (..)
    , Method (GET, POST)
    , Response
    , ServerPart
    , ServerPartT
    , dir
    , method
    , methodM
    , nullConf
    , nullDir
    , ok
    , path
    , seeOther
    , toResponse
    )
import Happstack.Server.SimpleHTTP
    ( simpleHTTP
    )
import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

helloHandler :: ServerPartT IO String
helloHandler =
    ok "Hello, World!"

byeHandler :: ServerPartT IO Response
byeHandler =
    ok $ toResponse "Goodbye, World!"

data Subject = World | Haskell

sayHello :: Subject -> String
sayHello World = "Hello, World!!"
sayHello Haskell = "Greetings, Haskell!"

instance FromReqURI Subject where
    fromReqURI = \case
        "world" -> Just World
        "haskell" -> Just Haskell
        _ -> Nothing

main2 :: IO ()
main2 = simpleHTTP nullConf $ ok (toResponse "Hello, World!")

main :: IO ()
main =
    let port' = 1234
     in do
            putStrLn $ "Listening on port " <> show port'
            simpleHTTP
                nullConf
                    { port = port'
                    }
                $ msum
                    [ mzero -- represents failure so the next handler will be tried
                    , dir "hello" $ dir "world" $ ok $ toResponse "Hello, Happstack!"
                    , dir "hello" $ path $ \s -> ok $ toResponse $ "Hello, " <> s
                    , dir "greet1" $ path $ \subject -> ok $ toResponse $ sayHello subject
                    , dir "greet2" $ path $ \subject -> ok . toResponse . sayHello $ subject
                    , dir "greet3" $ path $ ok . toResponse . sayHello
                    , dir "bye" $ dir "moon" $ ok $ toResponse "Goodbye, Moon!"
                    , dir "bye" byeHandler
                    , dir "hi" $ toResponse <$> helloHandler
                    , dir "foo" $ do
                        method GET
                        ok $ toResponse "You requested: /foo (over GET)"
                    , dir "bar" $ method POST >> ok (toResponse "You requested: /bar (over POST)")
                    , -- nullDir forces the whole dir segment to be consumed (/baz works, not /baz/wat)
                      dir "baz" $ method POST >> nullDir >> ok (toResponse "You requested: /baz (over POST)")
                    , dir "qux" (method GET >> ok (toResponse "You requested: /qux (over GET)"))
                    , dir "quux" (method GET >>= \() -> ok (toResponse "You requested: /quux (over GET)"))
                    , -- methodM implicitly calls nullDir, and will be deprecated...
                      dir "wat" $ methodM [POST, GET] >> ok (toResponse "wat")
                    , dir "blaze" $ method GET >> nullDir >> helloTmpl
                    , dir "hsx" $ method GET >> nullDir >> helloHsx
                    , seeOther
                        {- http --follow localhost:1234/wat -}
                        "/hello/stranger"
                        (toResponse "Redirecting...")
                    ]

layout :: String -> [Html] -> Html -> Html
layout title headers body = H.html $ do
    H.head $ do
        H.title $ H.toHtml title
        H.meta
            ! A.httpEquiv (H.toValue "Content-Type")
            ! A.content (H.toValue "text/html;charset=utf-8")
        sequence_ headers
    H.body $ do
        body

helloTmpl :: ServerPart Response
helloTmpl =
    ok $
        toResponse $
            layout
                "Hello, Blaze!"
                [ H.meta
                    ! A.name (H.toValue "keywords")
                    ! A.content (H.toValue "happstack, blaze, html")
                ]
                ( H.p $ do
                    H.string "Hello, "
                    H.b $ H.string "blaze-html!"
                )

-- Last compatible GHC version is from 2023... Do not use!
-- helloHsx :: ServerPartT IO XML
-- helloHsx =
--     unHSPT $
--         unXMLGenT
--             [hsx|
--   <html>
--    <head>
--     <title>Hello, HSP!</title>
--    </head>
--    <body>
--     <h1>Hello HSP!</h1>
--     <p>We can insert Haskell expression such as this:
--         <% show $ sum [1 .. (10 :: Int)] %></p>
--     <p>We can use the ServerPartT monad too.
--        Your request method was: <% getMethod %></p>
--     <hr/>
--     <p>We don't have to escape & or >. Isn't that nice?</p>
--     <p>If we want <% "<" %> then we have to do something funny.</p>
--     <p>But we don't have to worry about
--        escaping <% "<p>a string like this</p>" %></p>
--     <p>We can also nest <% <span>like <% "this." %> </span> %></p>
--    </body>
--   </html>
--  |]
--   where
--     getMethod :: XMLGenT (HSPT XML (ServerPartT IO)) String
--     getMethod = show . rqMethod <$> askRq