{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module MyLib (run) where

import Data.Time (Day, fromGregorianValid)
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.RawString.QQ (r)

data OrgItem
    = Scheduled Day
    | Deadline Day
    | Closed Day
    deriving (Show, Eq, Generic)

digits :: Int -> Parser String
digits n = count n digit

{-
>>> parse parseDate "" "2024-09-28"
Right 2024-09-28
 -}
date :: Parser Day
date = do
    year <- read <$> digits 4
    _ <- char '-'
    month <- read <$> digits 2
    _ <- char '-'
    day <- read <$> digits 2
    case fromGregorianValid year month day of
        Just d ->
            return d
        Nothing ->
            fail $ "Invalid date: " <> show (year, month, day)

{-
>>> parse weekday "" "Sun"
Right "Sun"

>>> parse weekday "" "Thu"
Right "Thu"

>>> parse weekday "" "Moon"
Left (line 1, column 1):
unexpected "M"
expecting "Sun", "Mon", "Tue", "Wed", "Thu", "Fri" or "Sat"
 -}
weekday :: Parser String
weekday =
    choice $
        fmap
            (try . string)
            [ "Sun"
            , "Mon"
            , "Tue"
            , "Wed"
            , "Thu"
            , "Fri"
            , "Sat"
            ]

ws :: Parser ()
ws = skipMany (oneOf " \t\n\r")

{-
>>> parse orgItem "" "SCHEDULED: <2024-09-28 Sun>"
Right (Scheduled 2024-09-28)

>>> parse orgItem "" "CLOSED: <2024-02-30 Sun>"
Left (line 1, column 20):
Invalid date: (2024,2,30)
 -}
orgItem :: Parser OrgItem
orgItem =
    choice
        [ string "SCHEDULED:" *> ws *> between (char '<') (char '>') (Scheduled <$> date <* ws <* weekday)
        , string "DEADLINE:" *> ws *> between (char '<') (char '>') (Deadline <$> date <* ws <* weekday)
        , string "CLOSED:" *> ws *> between (char '<') (char '>') (Closed <$> date <* ws <* weekday)
        ]

orgItems :: Parser [OrgItem]
orgItems =
    ws *> sepEndBy orgItem newline <* ws

input1 :: String
input1 =
    unlines
        [ "SCHEDULED: <2024-09-28 Sun>"
        , "DEADLINE: <2024-10-05 Sun>"
        , "CLOSED: <2024-10-05 Sun>"
        ]

input2 :: String
input2 =
    [r|
SCHEDULED: <2024-09-28 Sun>
DEADLINE:  <2024-10-05 Sun>
CLOSED:    <2024-10-05 Sun>
    |]

run :: IO ()
run = do
    print $ parse date "A" "2024-09-28"
    print $ parse orgItem "B" "SCHEDULED: <2024-09-28 Sun>"
    print $ parse orgItems "C" $ unlines ["SCHEDULED: <2024-09-28 Sun>", "SCHEDULED: <2024-09-28 Sun>"]
    print $ parse orgItems "D" input1
    print $ parse orgItems "E" input2
