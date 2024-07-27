{-# LANGUAGE OverloadedStrings #-}

{-

rg --files | entr -c runghc ./main.hs
rg --files | entr -c bash -c 'runghc ./main.hs|tidy -quiet'
rg --files | entr -c bash -c 'runghc ./main.hs | npx prettier --stdin-filepath bogus.html'

 -}

-- import Lucid (Html)
-- import Lucid.Html5

import Control.Monad (forM_)
import Lucid (
    Html,
    ToHtml (toHtml),
    body_,
    class_,
    div_,
    doctype_,
    h1_,
    head_,
    html_,
    lang_,
    li_,
    table_,
    td_,
    title_,
    tr_,
    ul_,
 )

myTitle :: Html ()
myTitle = h1_ "Welcome!"

view :: Int -> Html ()
view n =
    mconcat
        [ doctype_
        , html_ [lang_ "en", class_ "dark"] $ do
            head_ $ do
                title_ "Hello World App"
            body_ $ do
                div_ [class_ "container"] $ do
                    myTitle

                div_ [class_ "container"] $ do
                    table_ $
                        mapM_ (tr_ . td_ . toHtml . show) [1, 2, 3]

                div_ [class_ "container"] $ do
                    ul_ $
                        forM_
                            [1 .. n]
                            (li_ . toHtml . show)
        ]

main :: IO ()
main = do
    print $ view 3