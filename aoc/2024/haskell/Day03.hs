{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >=>" #-}

{- cabal:

build-depends: base
-}

{-

Terminal 1
==========
./repl
> :cmd return $ unlines [":!clear",":reload", "main"]

Terminal 2
==========
find *.hs | entr tmux send-keys -t aoc:0 Up Enter

 -}

module Day03 (main, char2int, advanceUntil) where

import Control.Applicative
import Data.Char (ord)
import Data.List (isPrefixOf)
import Prelude hiding (any)

main :: IO ()
main = do
    input <- readFile "../_inputs/03.txt"
    print $ answer1 input -- want: 178794710
    print $ answer2 input -- want: 76729637

answer1 :: String -> Int
answer1 str = case fst <$> runParser (many1' mul) str of
    Nothing -> error "failed to parse"
    Just multiplications ->
        foldl (\acc (Mul (a, b)) -> acc + a * b) 0 multiplications

answer2 :: String -> Int
answer2 str =
    case extractInstructions str of
        Nothing -> error "parse error"
        Just xs -> evalInstructions xs

newtype Parser a
    = Parser {runParser :: String -> Maybe (a, String)}

instance Show (Parser a) where
    show _ = "Parser <function>"

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> case p str of
        Nothing -> Nothing
        Just (x, rest) -> Just (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \str -> Just (x, str)
    (Parser p1) <*> (Parser p2) = Parser $ \str -> case p1 str of
        Nothing -> Nothing
        Just (f, rest1) -> case p2 rest1 of
            Nothing -> Nothing
            Just (x, rest2) -> Just (f x, rest2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \str -> case p1 str of
        Just x -> Just x
        Nothing -> p2 str

{-
>>> char2int <$> ['0' .. '9']
[0,1,2,3,4,5,6,7,8,9]
 -}
char2int :: Char -> Int
char2int c = ord c - ord '0'

{-

>>> runParser digit "123"
Just ('1',"23")

>>> runParser digit "abc"
Nothing

>>> runParser digit "1x2"
Just ('1',"x2")

>>> runParser digit "x12"
Nothing

 -}

digit :: Parser Char
digit = Parser $
    \case
        "" -> Nothing
        (x : xs) ->
            if x `elem` ['0' .. '9']
                then Just (x, xs)
                else Nothing

{-

>>> runParser (char 'a') "abc"
Just ('a',"bc")

>>> runParser (char 'b') "abc"
Nothing

 -}

char :: Char -> Parser Char
char c = Parser $ \case
    "" -> Nothing
    (x : xs)
        | x == c -> Just (x, xs)
        | otherwise -> Nothing

{-

>>> runParser (many1 digit) "abc"
Nothing

>>> runParser (many1 digit) "123"
Just ("123","")

>>> runParser (many1 digit) "1x23"
Just ("1","x23")

>>> runParser (many1 digit) "123x456"
Just ("123","x456")

 -}

many1 :: Parser a -> Parser [a]
many1 p = Parser $ \str ->
    runParser p str >>= \(x, rest1) ->
        case runParser (many1 p) rest1 of
            Nothing -> Just ([x], rest1)
            Just (xs, rest2) -> Just (x : xs, rest2)

{-

A variation of `many1` that discards any non-matching input. I tries to match
as much as possible.

>>> runParser (many1' digit) "abc"
Just ("","")

>>> runParser (many1' digit) "123"
Just ("123","")

>>> runParser (many1' digit) "1x23--456"
Just ("123456","")

>>> runParser (many1 mul) "mul(1,2)__mul(3,4]__mul(5,6)"
Just ([Mul (1,2)],"__mul(3,4]__mul(5,6)")

>>> runParser (many1' mul) "mul(1,2)__mul(3,4]__mul(5,6)"
Just ([Mul (1,2),Mul (5,6)],"")

 -}
many1' :: Parser a -> Parser [a]
many1' p = Parser $ \str -> aux [] str
  where
    aux acc "" = Just (reverse acc, "") -- End of input
    aux acc rest =
        case runParser p rest of
            Just (x, xs) -> aux (x : acc) xs -- Successful parse, continue
            Nothing ->
                case runParser advance rest of
                    Nothing -> Just (reverse acc, rest) -- Cannot advance, return what we have
                    Just (_, xs) -> aux acc xs -- Skip one character and retry

{-

>>> runParser int "123x456"
Just (123,"x456")

 -}

int :: Parser Int
int = fmap read (many1 digit)

{-

>>> runParser (ident "run") "runner"
Just ("run","ner")

>>> runParser (ident "run") "trunk"
Nothing

 -}
ident :: String -> Parser String
ident ref = Parser $ \str ->
    if ref `isPrefixOf` str
        then Just (ref, drop (length ref) str)
        else Nothing

{-

>>> runParser (ident "hello") "x-hello"
Nothing

>>> runParser (advance *> advance *> ident "hello") "x-hello"
Just ("hello","")

 -}

advance :: Parser ()
advance = Parser $ \case
    "" -> Nothing
    (_ : xs) -> Just ((), xs)

{-

>>> runParser (advanceUntil (\c -> c == 'h') *> ident "hello") "x-hello"
Just ("hello","")

 -}
advanceUntil :: (Char -> Bool) -> Parser ()
advanceUntil f = Parser $ \case
    "" -> Nothing
    (x : xs) ->
        if f x
            then Just ((), x : xs)
            else runParser (advanceUntil f) xs

{-

>>> runParser (untilP (char 'h') *> ident "hello") "x123-hello"
Just ("hello","")

See `untilP'` to reduce verbosity
>>> runParser (untilP (ident "mul(") *> ident "mul(" *> int <* char ')') "x123-hello__mul(123)"
Just (123,")")
 -}
untilP :: Parser a -> Parser ()
untilP p = Parser $ \case
    "" -> Nothing
    (x : xs) ->
        case runParser p (x : xs) of
            Nothing -> runParser (untilP p) xs
            Just _ -> Just ((), x : xs)

{-

Same as `untilP` but advances the cursor after the match (rather than stopping before the match)

>>> runParser (untilP' (ident "mul(") *> int <* char ')') "x123-hello__mul(123)"
Just (123,"")

 -}

untilP' :: Parser b -> Parser b
untilP' p = untilP p *> p

{-

>>> runParser mul "mul(123,456)"
Just (Mul (123,456),"")

>>> runParser mul "(123]mul(234,567)"
Just (Mul (234,567),"")

>>> runParser mul "(123]mul[234,567)"
Nothing

>>> runParser (many1 mul) "mul(123,456)--something**mul(234,567)"
Just ([Mul (123,456),Mul (234,567)],"")

 -}

newtype Mul = Mul (Int, Int)
    deriving (Show)

mul :: Parser Mul
mul =
    curry Mul
        <$> (ident "mul(" *> int)
        <*> (char ',' *> int <* char ')')

newtype Do = Do String
    deriving (Show)

do' :: Parser Do
do' = Do <$> ident "do()"

newtype Dont = Dont String
    deriving (Show)

dont :: Parser Dont
dont = Dont <$> ident "don't()"

data Instruction
    = MkDo Do
    | MkDont Dont
    | MkMul Mul
    deriving (Show)

instruction :: Parser Instruction
instruction =
    asum
        [ MkDo <$> do'
        , MkDont <$> dont
        , MkMul <$> mul
        ]

extractInstructions :: String -> Maybe [Instruction]
extractInstructions str = fst <$> runParser (many1 (untilP' instruction)) str

evalInstructions :: [Instruction] -> Int
evalInstructions xs =
    snd $ foldl step (True, 0) xs
  where
    step (isOn, total) instr = case instr of
        (MkDo _) -> (True, total)
        (MkDont _) -> (False, total)
        (MkMul (Mul (x, y))) -> (isOn, if isOn then total + x * y else total)

-- wipStr = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- wip :: IO ()
-- wip = case extractInstructions wipStr of
--     Nothing -> putStrLn "parse error"
--     Just xs -> do
--         mapM_ print xs
--         print $ evalInstructions xs
