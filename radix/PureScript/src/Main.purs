module Main (main) where

import Prelude

import Data.Char (toCharCode)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

foreign import mainLoop :: (Int -> String -> Effect Unit) -> Effect Unit
foreign import myLog :: String -> Effect Unit

toBase10 :: Char -> Maybe Int
toBase10 c
  | c >= '0' && c <= '9' = Just $ toCharCode c - toCharCode '0'
  | c >= 'A' && c <= 'Z' = Just $ toCharCode c - toCharCode 'A' + 10
  | c >= 'a' && c <= 'z' = Just $ toCharCode c - toCharCode 'a' + 10
  | otherwise = Nothing


convertToBase10 :: Int -> Array Char -> Tuple (List Int) (List Char)
convertToBase10 base =
  List.foldl
    (\(Tuple good bad) c ->
      case toBase10 c of
        Nothing -> Tuple good (c : bad)
        Just n ->
          if n < 0 || n >= base || base < 2 || base > 36
            then Tuple good (c : bad)
            else Tuple (n : good) bad
    ) (Tuple Nil Nil)

horner :: Int -> List Int -> Int
horner base =
  List.foldr
    (\n acc -> n + acc * base)
    0

handleLine :: Int -> String -> Effect Unit
handleLine base line = do
  case convertToBase10 base (toCharArray line) of
    Tuple good Nil -> do
      log $ show good
      log $ " -> " <> show (horner base good)
    Tuple _ bad -> do
      log $ "Bad chars: " <> show bad
  log ""


{-
Run or build with:
  spago run --watch -l
  spago build --watch -l


Run with:
clear && spago bundle-app --platform=node && node . 16

 -}
main :: Effect Unit
main = do
  log "Booting up..."
  log "Checking internals..."
  myLog "Ready!\n"
  mainLoop handleLine