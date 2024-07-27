{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Blaze.Html ((!))
import Text.Blaze.Html.Renderer.Pretty qualified as Pretty
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

bodyContent :: Int -> Html
bodyContent n = do
    H.h1 "Listing numbers"

    H.div ! A.class_ "number-list" $ do
        H.h2 "First list"
        H.ul $ forM_ [1 .. n] $ H.li . H.toHtml

    H.div ! A.class_ "number-list" $ do
        H.h2 "Second list"
        H.ul $ mapM_ (H.li . H.toHtml) [1 .. n]

mainHtml :: Int -> Html
mainHtml n = H.docTypeHtml $ do
    H.head $ do
        H.title "Numbers app"

    H.body $ do
        bodyContent n

{-

rg --files | entr -c runghc ./main.hs

 -}
main :: IO ()
main = do
    {-

    Reminder
    ========

    `mapM_` allows mapping over values in a monadic context, but discarding the result

    See this repo: gpw-haskell/lesson22/02_get_lines.hs

    `forM_` is `mapM_` flipped

     -}
    mapM_ print [1, 2, 3]
    forM_ [1, 2, 3] print
    putStrLn "---"
    putStrLn $ Pretty.renderHtml $ mainHtml 3